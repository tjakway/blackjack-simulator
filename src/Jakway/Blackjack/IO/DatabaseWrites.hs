module Jakway.Blackjack.IO.DatabaseWrites where

import Jakway.Blackjack.Visibility
import Jakway.Blackjack.Cards
import Jakway.Blackjack.CardOps
import Jakway.Blackjack.Result
import Jakway.Blackjack.Match
import Jakway.Blackjack.IO.DatabaseCommon
import Jakway.Blackjack.Util
import Database.HDBC
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as HashMap

enableForeignKeys :: IConnection a => a -> IO Integer
enableForeignKeys conn = run conn "PRAGMA foreign_keys = ON;" []

flipInner2 :: (a -> b -> c -> d) -> a -> c -> b -> d
flipInner2 f x y z = f x z y

createTables :: IConnection a => a -> TableNames -> IO ()
createTables conn tableNames =
            sequence_ $ map (flipInner2 run conn []) createTableStatements
        where createTableStatements = [ "CREATE TABLE IF NOT EXISTS cards (id INTEGER PRIMARY KEY AUTOINCREMENT, cardValue INTEGER NOT NULL, suit INTEGER NOT NULL, visible INTEGER NOT NULL)",
                                      -- ^ Sqlite doesn't have a boolean
                                      -- data type, see https://www.sqlite.org/datatype3.html and http://stackoverflow.com/questions/843780/store-boolean-value-in-sqlite
                                      -- ****IMPORTANT****
                                      -- SQL doesn't allow dynamic table
                                      -- names so they have to be manually
                                      -- inserted
                                      -- this is technically a SQL
                                      -- injection vulnerability but it's
                                      -- not like we have any malicious
                                      -- users...
                                        "CREATE TABLE "++ (getPlayerTableName tableNames) ++" (whichPlayer INTEGER PRIMARY KEY)",
                                        -- | Note that SQLite doesn't allow
                                        -- any datatype other than INTEGER
                                        -- to be declared PRIMARY KEY
                                        -- AUTOINCREMENT
                                        "CREATE TABLE "++ (getHandTableName tableNames)  ++" (id INTEGER PRIMARY KEY AUTOINCREMENT, whichPlayer INTEGER, whichHand BIGINT, thisCard INTEGER, "
                                                            ++ "FOREIGN KEY(whichPlayer) REFERENCES players(whichPlayer), FOREIGN KEY(thisCard) REFERENCES cards(id) )",
                                        "CREATE TABLE "++ (getMatchTableName tableNames) ++"(id INTEGER PRIMARY KEY AUTOINCREMENT, whichGame INTEGER, dealersHand BIGINT, whichPlayer INTEGER, thisPlayersHand BIGINT, playerResult INTEGER, " ++
                                                              "FOREIGN KEY(dealersHand) REFERENCES hands(whichHand), FOREIGN KEY(whichPlayer) REFERENCES players(whichPlayer), FOREIGN KEY(thisPlayersHand) REFERENCES hands(whichHand) ) " ]

initializeDatabase :: (IConnection a) => a -> [TableNames]-> IO ()
initializeDatabase conn allTableNames = enableForeignKeys conn >> mapM_ (createTables conn) allTableNames

--there's only one cards table
insertCardStatement :: (IConnection a) => a -> IO (Statement)
--ignore the id field
insertCardStatement conn = prepare conn ("INSERT INTO cards(id, cardValue, suit, visible) VALUES(?, ?, ?, ?)")


insertAllCards :: (IConnection a) => a -> IO ()
insertAllCards conn = do 
                         -- ^ newDeck is a (sorted) array of all possible card values
                         insertStatement <- insertCardStatement conn
                         executeMany insertStatement cardsSqlValues

insertPlayerStatement :: (IConnection a) => a -> TableNames -> IO (Statement)
insertPlayerStatement conn tableNames = prepare conn ("INSERT INTO " ++ playersTable ++ "(whichPlayer) VALUES(?)")
                        where playersTable = getPlayerTableName tableNames

-- |note: include the dealer in the number of players you're inserting
-- if you pass 1 you won't have anyone to play against
insertPlayers :: (IConnection a) => a -> TableNames -> Int -> IO ()
insertPlayers conn tableNames numPlayers = insertPlayerStatement conn tableNames >>= (\insertStatement -> executeMany insertStatement insValues)
                        where insValues = map ((: []) . toSql) [0..(numPlayers-1)]
                            -- ^ player number 0 is the dealer!

insertHandStatement :: (IConnection a) => a -> TableNames -> IO (Statement)
insertHandStatement conn tableNames = prepare conn ("INSERT INTO " ++ handTable ++ "(whichPlayer, whichHand, thisCard) VALUES(?, ?, ?)")
                        where handTable = getHandTableName tableNames

-- |need this function because insertHand returns the whichHand value of the inserted
-- hand
-- there are many fewer whichHand values than id values because one hand is
-- spread over many columns (one per card)
nextHandId :: (IConnection a) => a -> TableNames -> IO (Integer)
nextHandId conn tableNames = do
                    resArray <- quickQuery' conn ("SELECT MAX(whichHand) FROM " ++ handTable) []
                    let res = ((resArray !! 0) !! 0)
                    if res == SqlNull then return 0
                                      else return . (+1) . fromSql $ res
                where handTable = getHandTableName tableNames

handToSqlValues :: Int -> Integer -> Hand -> [[SqlValue]]
handToSqlValues whichPlayer handId hand = map (\thisCard -> [toSql whichPlayer, toSql handId, toSql $ fromJust $ cardToForeignKeyId thisCard]) hand

insertHand :: (IConnection a) => Statement -> a -> TableNames -> Int -> Hand -> IO (Integer)
-- use the connection to figure out what whichHand ID to assign this hand
-- (this is NOT the database id column!)
-- before running this, need to build the cards table, then run
-- insertHandStatement once for every card in this hand, passing the rowId
-- of the corresponding card for thisCard 
insertHand insertStatement conn tableNames whichPlayer hand = do
        handId <- nextHandId conn tableNames
        -- |if cardtoForeignKeyId returns Nothing it's an unrecoverable
        -- error anyways
        let values = handToSqlValues whichPlayer handId hand
        executeMany insertStatement values
        -- return the handId we inserted
        -- you can get the hand back by querying all rows where whichHand == handId
        return handId


-- |Should this return the player ids with each hand id in a tuple?
-- FIXME: change the tuple to 2 separate parameters
insertHands :: (IConnection a) => Statement -> a -> TableNames -> ([Int], [Hand]) -> IO ([Integer])
insertHands insertStatement conn tableNames (whichPlayers, []) = return []
insertHands insertStatement conn tableNames (whichPlayers, hands) = do
       handId <- nextHandId conn tableNames
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
nextGameId :: (IConnection a) => a -> TableNames -> IO (Integer)
nextGameId conn tableNames = do
                    resArray <- quickQuery' conn ("SELECT MAX(whichGame) FROM " ++ matchesTable) []
                    let res = ((resArray !! 0) !! 0)
                    if res == SqlNull then return 0
                                      else return . (+1) . fromSql $ res
                where matchesTable = getMatchTableName tableNames

insertMatchStatement :: (IConnection a) => a -> TableNames -> IO (Statement)
insertMatchStatement conn tableNames = prepare conn ("INSERT INTO " ++ matchesTable ++ " (whichGame, dealersHand, whichPlayer, thisPlayersHand, playerResult) VALUES(?, ?, ?, ?, ?)")
                where matchesTable = getMatchTableName tableNames

insertMatch :: (IConnection a) => Statement -> Statement -> a -> TableNames -> Match -> IO (Integer)
insertMatch insMatchStatement insHandStatement conn tableNames (Match dealersHand  playerIds playerHands playerResults) = do
    gameId <- nextGameId conn tableNames

    --FIXME: dealer's ID is assumed to be 0!  Change if rewriting this
    dealersHandId <- insertHand insHandStatement conn tableNames 0 dealersHand

    insertedHandIds <- insertHands insHandStatement conn tableNames (playerIds, playerHands)
    let values = map (\(hId, pId, pRes) -> map toSql $ [gameId, dealersHandId, hId, toInteger pId, toInteger . fromEnum $ pRes]) $ zip3 insertedHandIds playerIds playerResults
    executeMany insMatchStatement values
    return gameId
