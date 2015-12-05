module Jakway.Blackjack.IO.Database where

import Jakway.Blackjack.Visibility
import Jakway.Blackjack.Cards
import Jakway.Blackjack.CardOps
import Database.HDBC
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as HashMap

enableForeignKeys :: IConnection a => a -> IO Integer
enableForeignKeys conn = run conn "PRAGMA foreign_keys = ON;" []

flipInner2 :: (a -> b -> c -> d) -> a -> c -> b -> d
flipInner2 f x y z = f x z y

createTables :: IConnection a => a -> IO ()
createTables conn =
            sequence_ $ map (flipInner2 run conn []) createTableStatements
        where createTableStatements = [ "CREATE TABLE cards (id INTEGER PRIMARY KEY AUTOINCREMENT, cardValue INTEGER NOT NULL, suit INTEGER NOT NULL, visible INTEGER NOT NULL)",
                                      -- ^ Sqlite doesn't have a boolean
                                      -- data type, see https://www.sqlite.org/datatype3.html and http://stackoverflow.com/questions/843780/store-boolean-value-in-sqlite
                                        "CREATE TABLE players (whichPlayer INTEGER PRIMARY KEY)",
                                        "CREATE TABLE hands (id INTEGER PRIMARY KEY AUTOINCREMENT, whichPlayer INTEGER, whichHand INTEGER, thisCard INTEGER, "
                                                            ++ "FOREIGN KEY(whichPlayer) REFERENCES players(whichPlayer), FOREIGN KEY(thisCard) REFERENCES cards(id) )",
                                        "CREATE TABLE matches (id INTEGER PRIMARY KEY AUTOINCREMENT, whichGame INTEGER, dealersHand INTEGER, thisPlayersHand INTEGER, playerResult INTEGER)" ]

initializeDatabase :: (IConnection a) => a -> IO ()
initializeDatabase conn = enableForeignKeys conn >> createTables conn

insertCardStatement :: (IConnection a) => a -> IO (Statement)
--ignore the id field
insertCardStatement conn = prepare conn "INSERT INTO cards(id, cardValue, suit, visible) VALUES(?, ?, ?, ?)"

-- | XXX: for some reason this function wouldn't work in a where binding?
cardSqlArr :: Suit -> CardValue -> [SqlValue]
cardSqlArr s v = [toSql . fromEnum $ v, toSql . fromEnum $ s]
singleCardToSqlValues :: Visibility Card -> [SqlValue]
singleCardToSqlValues (Shown (Card suit val))   = (cardSqlArr suit val) ++ [iToSql 0]
singleCardToSqlValues (Hidden (Card suit val))  = (cardSqlArr suit val) ++ [iToSql 1]

cardPermutations :: [Visibility Card]
cardPermutations = (Shown <$> newDeck) ++ (Hidden <$> newDeck) 

cardIdMap :: HashMap.Map (Visibility Card) Int
cardIdMap = HashMap.fromList $ zip cardPermutations ids
    where ids = [1..(length cardPermutations)]


cardToForeignKeyId :: Visibility Card -> Maybe Int
cardToForeignKeyId card = HashMap.lookup card cardIdMap

cardsSqlValues :: [[SqlValue]]
cardsSqlValues = map (\ (id, cardSqlValues) -> (toSql id) : cardSqlValues) (zip ids cardsWithoutIds)
    where cardsWithoutIds = singleCardToSqlValues <$> cardPermutations
          -- |SQL ids count up from 1
          ids = [1..(length cardsWithoutIds)]


insertAllCards :: (IConnection a) => a -> IO ()
insertAllCards conn = do 
                         -- ^ newDeck is a (sorted) array of all possible card values
                         insertStatement <- insertCardStatement conn
                         executeMany insertStatement cardsSqlValues

insertPlayerStatement :: (IConnection a) => a -> IO (Statement)
insertPlayerStatement conn = prepare conn "INSERT INTO players(whichPlayer) VALUES(?)"

insertPlayers :: (IConnection a) => a -> Int -> IO ()
insertPlayers conn numPlayers = insertPlayerStatement conn >>= (\insertStatement -> executeMany insertStatement insValues)
                        where insValues = map ((: []) . toSql) [0..(numPlayers-1)]
                            -- ^ player number 0 is the dealer!

insertHandStatement :: (IConnection a) => a -> IO (Statement)
insertHandStatement conn = prepare conn "INSERT INTO hands(whichPlayer, whichHand, thisCard) VALUES(?, ?, ?)"

-- |need this function because insertHand returns the whichHand value of the inserted
-- hand
-- there are many fewer whichHand values than id values because one hand is
-- spread over many columns (one per card)
nextHandId :: (IConnection a) => a -> IO (Int)
nextHandId conn = do
                    resArray <- quickQuery' conn "SELECT MAX(whichHand) FROM hands" []
                    let res = ((resArray !! 0) !! 0)
                    if res == SqlNull then return 0
                                      else return . fromSql $ res

insertHand :: (IConnection a) => Statement -> a -> Int -> Hand -> IO (Int)
-- use the connection to figure out what whichHand ID to assign this hand
-- (this is NOT the database id column!)
-- before running this, need to build the cards table, then run
-- insertHandStatement once for every card in this hand, passing the rowId
-- of the corresponding card for thisCard 
insertHand insertStatement conn whichPlayer hand = do
        handId <- nextHandId conn
        -- |if cardtoForeignKeyId returns Nothing it's an unrecoverable
        -- error anyways
        let values = map (\thisCard -> [toSql whichPlayer, toSql handId, toSql $ fromJust $ cardToForeignKeyId thisCard]) hand
        executeMany insertStatement values
        -- return the handId we inserted
        -- you can get the hand back by querying all rows where whichHand == handId
        return handId
