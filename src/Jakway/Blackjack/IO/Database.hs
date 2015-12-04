module Jakway.Blackjack.IO.Database where

import Jakway.Blackjack.Visibility
import Jakway.Blackjack.Cards
import Database.HDBC

enableForeignKeys :: IConnection a => a -> IO Integer
enableForeignKeys conn = run conn "PRAGMA foreign_keys = ON;" []

flipInner2 :: (a -> b -> c -> d) -> a -> c -> b -> d
flipInner2 f x y z = f x z y

createTables :: IConnection a => a -> IO ()
createTables conn =
            sequence_ $ map (flipInner2 run conn []) createTableStatements
        where createTableStatements = [ "CREATE TABLE cards (id INTEGER PRIMARY KEY AUTOINCREMENT, cardValue INTEGER NOT NULL, suit INTEGER NOT NULL, visibile INTEGER NOT NULL)",
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
insertCardStatement conn = prepare conn "INSERT INTO cards(cardValue, suit) VALUES(?, ?)"

-- | XXX: for some reason this function wouldn't work in a where binding?
cardSqlArr :: Suit -> CardValue -> [SqlValue]
cardSqlArr s v = [toSql . fromEnum $ v, toSql . fromEnum $ s]
cardToSqlValues :: Visibility Card -> [SqlValue]
cardToSqlValues (Shown (Card suit val))   = (cardSqlArr suit val) ++ [iToSql 0]
cardToSqlValues (Hidden (Card suit val))  = (cardSqlArr suit val) ++ [iToSql 1]


insertAllCards :: (IConnection a) => a -> IO ()
insertAllCards conn = do 
                         let cardSqlValues = cardToSqlValues <$> (Shown <$> newDeck) ++ (Hidden <$> newDeck) 
                             -- ^ newDeck is a (sorted) array of all possible card values
                         insertStatement <- insertCardStatement conn
                         executeMany insertStatement cardSqlValues

insertPlayerStatement :: (IConnection a) => a -> IO (Statement)
insertPlayerStatement conn = prepare conn "INSERT INTO players(whichPlayer) VALUES(?)"

insertPlayers :: (IConnection a) => a -> Int -> IO ()
insertPlayers conn numPlayers = insertPlayerStatement conn >>= (\insertStatement -> executeMany insertStatement insValues)
                        where insValues = map ((: []) . toSql) [0..(numPlayers-1)]
                            -- ^ player number 0 is the dealer!

insertHandStatement :: (IConnection a) => a -> IO (Statement)
insertHandStatement conn = prepare conn "INSERT INTO hands(whichPlayer, whichHand, thisCard) VALUES(?, ?, ?)"

nextHandId :: (IConnection a) => a -> IO Int
nextHandId conn = do
                    resArray <- quickQuery' conn "SELECT MAX(whichHand) FROM hands" []
                    let res = ((resArray !! 0) !! 0)
                    if res == SqlNull then return 0
                                      else return . fromSql $ res

insertHand :: (IConnection a) => Statement -> a -> Int -> IO (Int)
-- use the connection to figure out what ID to assign this hand
-- before running this, need to build the cards table, then run
-- insertHandStatement once for every card in this hand, passing the rowId
-- of the corresponding card for thisCard 
insertHand statement conn whichPlayer = undefined
