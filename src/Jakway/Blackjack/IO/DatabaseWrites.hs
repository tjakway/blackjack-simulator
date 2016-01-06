module Jakway.Blackjack.IO.DatabaseWrites where

import Jakway.Blackjack.Visibility
import Jakway.Blackjack.Cards
import Jakway.Blackjack.CardOps
import Jakway.Blackjack.Result
import Jakway.Blackjack.IO.DatabaseCommon
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
                                        -- | Note that SQLite doesn't allow
                                        -- any datatype other than INTEGER
                                        -- to be declared PRIMARY KEY
                                        -- AUTOINCREMENT
                                        "CREATE TABLE hands (id INTEGER PRIMARY KEY AUTOINCREMENT, whichPlayer INTEGER, whichHand BIGINT, thisCard INTEGER, "
                                                            ++ "FOREIGN KEY(whichPlayer) REFERENCES players(whichPlayer), FOREIGN KEY(thisCard) REFERENCES cards(id) )",
                                        "CREATE TABLE matches (id INTEGER PRIMARY KEY AUTOINCREMENT, whichGame INTEGER, dealersHand BIGINT, whichPlayer INTEGER, thisPlayersHand BIGINT, playerResult INTEGER, " ++
                                                              "FOREIGN KEY(dealersHand) REFERENCES hands(whichHand), FOREIGN KEY(whichPlayer) REFERENCES players(whichPlayer), FOREIGN KEY(thisPlayersHand) REFERENCES hands(whichHand) ) " ]

initializeDatabase :: (IConnection a) => a -> IO ()
initializeDatabase conn = enableForeignKeys conn >> createTables conn

insertCardStatement :: (IConnection a) => a -> IO (Statement)
--ignore the id field
insertCardStatement conn = prepare conn "INSERT INTO cards(id, cardValue, suit, visible) VALUES(?, ?, ?, ?)"



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
nextHandId :: (IConnection a) => a -> IO (Integer)
nextHandId conn = do
                    resArray <- quickQuery' conn "SELECT MAX(whichHand) FROM hands" []
                    let res = ((resArray !! 0) !! 0)
                    if res == SqlNull then return 0
                                      else return . (+1) . fromSql $ res

handToSqlValues :: Int -> Integer -> Hand -> [[SqlValue]]
handToSqlValues whichPlayer handId hand = map (\thisCard -> [toSql whichPlayer, toSql handId, toSql $ fromJust $ cardToForeignKeyId thisCard]) hand

insertHand :: (IConnection a) => Statement -> a -> Int -> Hand -> IO (Integer)
-- use the connection to figure out what whichHand ID to assign this hand
-- (this is NOT the database id column!)
-- before running this, need to build the cards table, then run
-- insertHandStatement once for every card in this hand, passing the rowId
-- of the corresponding card for thisCard 
insertHand insertStatement conn whichPlayer hand = do
        handId <- nextHandId conn
        -- |if cardtoForeignKeyId returns Nothing it's an unrecoverable
        -- error anyways
        let values = handToSqlValues whichPlayer handId hand
        executeMany insertStatement values
        -- return the handId we inserted
        -- you can get the hand back by querying all rows where whichHand == handId
        return handId


{-FIXME: NEED TO CHANGE whichPlayer to whichPlayers
    When inserting multiple hands, need to have the player id for each hand too-}
-- |Should this return the player ids with each hand id in a tuple?
insertHands :: (IConnection a) => Statement -> a -> ([Int], [Hand]) -> IO ([Integer])
insertHands insertStatement conn (whichPlayers, []) = return []
insertHands insertStatement conn (whichPlayers, hands) = do
       handId <- nextHandId conn
       -- |don't query the database for hand we'll insert
       -- since nextHandId just returns the next highest available hand ID
       -- we can just keep incrementing that
       -- the downside to this approach is that we can never have multiple
       -- insertHands running simultaneously (though for real
       -- multithreading we'd have to make nextHandId atomic anyways)
       let handIds = ([handId.. (handId + (toInteger $ (length hands) - 1))]) :: [Integer]
       let values = map (\(thisPlayerId, thisHandId, thisHand) -> handToSqlValues thisPlayerId thisHandId thisHand) $ zip3 whichPlayers handIds hands
       sequence $ map (executeMany insertStatement) values
       return handIds

-- |like nextHandId but for whichGame
nextGameId :: (IConnection a) => a -> IO (Integer)
nextGameId conn = do
                    resArray <- quickQuery' conn "SELECT MAX(whichGame) FROM matches" []
                    let res = ((resArray !! 0) !! 0)
                    if res == SqlNull then return 0
                                      else return . (+1) . fromSql $ res

insertMatchStatement :: (IConnection a) => a -> IO (Statement)
insertMatchStatement conn = prepare conn "INSERT INTO matches (whichGame, dealersHand, whichPlayer, thisPlayersHand, playerResult) VALUES(?, ?, ?, ?, ?)"

insertMatch :: (IConnection a) => Statement -> Statement -> a -> Hand -> ([Hand], [Int], [Result]) -> IO ()
insertMatch insMatchStatement insHandStatement conn dealersHand (playerHands, playerIds, playerResults) = do
    gameId <- nextGameId conn

    --FIXME: dealer's ID is assumed to be 0!  Change if rewriting this
    dealersHandId <- insertHand insHandStatement conn 0 dealersHand

    insertedHandIds <- insertHands insHandStatement conn (playerIds, playerHands)
    let values = map (\(hId, pId, pRes) -> map toSql $ [gameId, dealersHandId, hId, toInteger pId, toInteger . fromEnum $ pRes]) $ zip3 insertedHandIds playerIds playerResults
    executeMany insMatchStatement values
