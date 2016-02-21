{-# LANGUAGE CPP #-}
module Jakway.Blackjack.IO.DatabaseWrites where

import Jakway.Blackjack.CardOps
import Jakway.Blackjack.Match
import Jakway.Blackjack.IO.DatabaseCommon
import Jakway.Blackjack.IO.TableNames
import Database.HDBC
import Data.Maybe (fromJust)

--PostgreSQL doesn't support PRAGMA syntax and will throw an error
enableForeignKeys :: IConnection a => a -> IO Integer
#ifdef BUILD_POSTGRESQL
--don't do anything, postgres enables foreign keys by default
enableForeignKeys _ = return 0 
#else
enableForeignKeys conn = run conn "PRAGMA foreign_keys = ON;" []
#endif

initializeDatabase :: (IConnection a) => a -> [TableNames]-> IO ()
initializeDatabase conn allTableNames = enableForeignKeys conn >> mapM_ (createTables conn) allTableNames >> commit conn

--there's only one cards table
insertCardStatement :: (IConnection a) => a -> IO (Statement)
--ignore the id field
insertCardStatement conn = prepare conn ("INSERT INTO cards(id, cardValue, suit, visible) VALUES(?, ?, ?, ?)")


insertAllCards :: (IConnection a) => a -> IO ()
insertAllCards conn = do 
                         -- ^ newDeck is a (sorted) array of all possible card values
                         insertStatement <- insertCardStatement conn
                         executeMany insertStatement cardsSqlValues
                         commit conn

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
                    let res = head . head $ resArray
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
insertHands _ _ _ (_, []) = return []
insertHands insertStatement conn tableNames (whichPlayers, hands) = do
       handId <- nextHandId conn tableNames
       -- |don't query the database for hand we'll insert
       -- since nextHandId just returns the next highest available hand ID
       -- we can just keep incrementing that
       -- the downside to this approach is that we can never have multiple
       -- insertHands running simultaneously (though for real
       -- multithreading we'd have to make nextHandId atomic anyways)
       let handIds = ([handId.. (handId + toInteger (length hands) - 1)]) :: [Integer]
       let values = map (\(thisPlayerId, thisHandId, thisHand) -> handToSqlValues thisPlayerId thisHandId thisHand) $ zip3 whichPlayers handIds hands
       mapM_ (executeMany insertStatement) values
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
insertMatch insMatchStatement insHandStatement conn tableNames (Match dHand pIds pHands pResults) = do
    gameId <- nextGameId conn tableNames

    --FIXME: dealer's ID is assumed to be 0!  Change if rewriting this
    dealersHandId <- insertHand insHandStatement conn tableNames 0 dHand
    commit conn

    insertedHandIds <- insertHands insHandStatement conn tableNames (pIds, pHands)
    commit conn
    let values = map (\(hId, pId, pRes) -> map toSql [gameId, dealersHandId, toInteger pId, hId, toInteger . fromEnum $ pRes]) $ zip3 insertedHandIds pIds pResults
    executeMany insMatchStatement values
    commit conn
    return gameId
