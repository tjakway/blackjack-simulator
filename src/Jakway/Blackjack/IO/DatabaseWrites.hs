{-# LANGUAGE CPP #-}
module Jakway.Blackjack.IO.DatabaseWrites where

import Jakway.Blackjack.CardOps
import Jakway.Blackjack.Match
import Jakway.Blackjack.AI
import Jakway.Blackjack.IO.DatabaseCommon
import qualified Jakway.Blackjack.IO.RDBMS.Postgres as PSQL
import Jakway.Blackjack.IO.TableNames
import Database.HDBC
import Data.Maybe (fromJust)

initializeDatabase :: (IConnection a) => a -> IO ()
initializeDatabase = withTransaction >>= PSQL.initialize

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

insertPlayerStatement :: (IConnection a) => a -> IO (Statement)
insertPlayerStatement conn tableNames = prepare conn ("INSERT INTO " ++ playersTable ++ "(whichPlayer, aiType) VALUES(?, ?)")
                        where playersTable = getPlayerTableName tableNames

-- |note: include the dealer in the number of players you're inserting
-- if you pass 1 you won't have anyone to play against
insertPlayers :: (IConnection a) => a -> Integer -> AI -> [AI] -> IO ()
insertPlayers conn runId dealerAI playerAIs = insertPlayerStatement conn tableNames >>= (\insertStatement -> executeMany insertStatement insValues)
                        where numPlayers = length playerAIs
                              dealerAIVal = toSql . show $ dealerAI
                              playerStrVals = map (toSql . show) playerAIs
                              playerIdVals = map iToSql [1..(numPlayers)]
                              playerVals = map (\(a, b) -> [a, b]) $ zip playerIdVals playerStrVals
                              insValues = ([iToSql 0, dealerAIVal] :  playerVals) :: [[SqlValue]]
                              -- ^ player number 0 is the dealer!

-- TODO: rewrite to use the SQL function next_hand_id
insertHandStatement :: (IConnection a) => a -> IO (Statement)
insertHandStatement conn tableNames = prepare conn ("INSERT INTO " ++ handTable ++ "(whichPlayer, whichHand, thisCard) VALUES(?, ?, ?)")
                        where handTable = getHandTableName tableNames
-- XXX: replace with SQL function next_hand_id and delete
-- *************************************************
-- |need this function because insertHand returns the whichHand value of the inserted
-- hand
-- there are many fewer whichHand values than id values because one hand is
-- spread over many columns (one per card)
nextHandId :: (IConnection a) => a -> IO (Integer)
nextHandId conn tableNames = do
                    resArray <- quickQuery' conn ("SELECT MAX(whichHand) FROM " ++ handTable) []
                    let res = head . head $ resArray
                    if res == SqlNull then return 0
                                      else return . (+1) . fromSql $ res
                where handTable = getHandTableName tableNames

handToSqlValues :: Int -> Integer -> Hand -> [[SqlValue]]
handToSqlValues whichPlayer handId hand = map (\thisCard -> [toSql whichPlayer, toSql handId, toSql $ fromJust $ cardToForeignKeyId thisCard]) hand

insertHand :: (IConnection a) => Statement -> a -> Integer -> Integer -> Int -> Hand -> IO (Integer)
-- use the connection to figure out what whichHand ID to assign this hand
-- (this is NOT the database id column!)
-- before running this, need to build the cards table, then run
-- insertHandStatement once for every card in this hand, passing the rowId
-- of the corresponding card for thisCard 
insertHand insertStatement conn runId matchId whichPlayer hand = do
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

insertMatchStatement :: (IConnection a) => a -> IO (Statement)
insertMatchStatement conn tableNames = prepare conn ("INSERT INTO " ++ matchesTable ++ " (whichGame, dealersHand, whichPlayer, thisPlayersHand, playerResult) VALUES(?, ?, ?, ?, ?)")
                where matchesTable = getMatchTableName tableNames

insertRunStatement :: (IConnection a) => a -> IO (Statement)
insertRunStatement conn = prepare conn "INSERT INTO runs DEFAULT VALUES RETURNING run_id;"

insertNewRun :: (IConnection a) => a -> Statement -> IO (Integer)
insertNewRun insRunStatement = do
        rows_modified <- execute insRunStatement []
        return $ assert (rows_modified == 1) rows_modified
        fetchAllRows' insRunStatement >>= return . fromSql . head . join 

insertMatch :: (IConnection a) => Statement -> Statement -> a -> Integer -> Match -> IO (Integer)
insertMatch insHandStatement insMatchStatement runId conn (Match dHand pIds pHands pResults) = do
    --FIXME: dealer's ID is assumed to be 0!  Change if rewriting this
    dealersHandId <- insertHand insHandStatement conn tableNames 0 dHand
    commit conn

    insertedHandIds <- insertHands insHandStatement conn tableNames (pIds, pHands)
    commit conn
    let values = map (\(hId, pId, pRes) -> map toSql [gameId, dealersHandId, toInteger pId, hId, toInteger . fromEnum $ pRes]) $ zip3 insertedHandIds pIds pResults
    executeMany insMatchStatement values
    commit conn
    return gameId


prepRun insRunStatement conn dealerAI playerAIs = do
            thisRunId <- insertNewRow insRunStatement
            insertPlayers conn dealerAI playerAIs
            return thisRunId



insertGame :: (IConnection a) => Statement -> Statement -> Statement -> a -> AI -> [AI] -> [Match] -> IO (Integer)
insertGame insRunStatement insPlayerStatement insHandStatement insMatchStatement p_conn dealerAI playerAIs (Match dHand pIds pHands pResults) = 
        withTransaction $ \conn -> do
            thisRunId <- prepRun



