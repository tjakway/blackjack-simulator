{-# LANGUAGE FlexibleContexts #-}
module Jakway.Blackjack.IO.DatabaseReads 
(readPlayers,
 readHandStatement,
 readHand,
 readPlayerHands,
 getNumPlayers,
 readMatchStatement,
 readMatch,
 getNumMatches
)
where

import Prelude hiding (lookup)
import Jakway.Blackjack.CardOps
import Jakway.Blackjack.Match
import Jakway.Blackjack.IO.DatabaseCommon
import Jakway.Blackjack.IO.TableNames
import Jakway.Blackjack.Result
import Jakway.Blackjack.Util (innerMapTuple4, ssub)
import Database.HDBC
import qualified Data.Map.Strict as HashMap
import Data.Maybe (fromJust, isNothing)
import Control.Monad (join, liftM)

-- |returns the number of matches from the database
-- it's an error to call getNumMatches on a table that hasn't been created yet
getNumMatches :: (IConnection a) => a -> TableNames -> IO (Integer)
getNumMatches conn tableNames = quickQuery' conn queryStr [] >>= (return . join) >>= 
        --if there aren't any rows then the number of matches is 0
        (\res -> case res of [] -> return 0
                             [x] -> return . fromSql $ x
                             _ -> error $ "Error in getNumMatches: unrecognized value returned from database for query " ++ queryStr)
                    --select only unique rows because the actual number of
                    --matches is the whichGame column
                    --any match with more than 2 players will take up more
                    --than 1 row in the matches table
                    --see http://stackoverflow.com/questions/11250253/postgresql-countdistinct-very-slow
                    where queryStr = ssub "SELECT COUNT(*) FROM (SELECT DISTINCT whichGame FROM ?) AS temp" [(getMatchTableName tableNames)]

readPlayers :: (IConnection a) => a -> TableNames -> IO ([Int])
readPlayers conn tableNames = do
        values <- quickQuery' conn ("SELECT whichPlayer FROM " ++ playerTable) []
        -- |quickQuery' returns [[SqlValue]].  Collapse the list.
        (return . (map fromSql)) (join values)
        where playerTable = getPlayerTableName tableNames

readHandStatement :: (IConnection a) => a -> TableNames -> IO (Statement)
readHandStatement conn tableNames = prepare conn $ "SELECT thisCard FROM " ++ handTable ++ " WHERE whichHand=?"
        where handTable = getHandTableName tableNames

readHand :: Statement -> Integer -> IO (Maybe Hand)
readHand statement whichHand = do
        _ <- execute statement [toSql whichHand]
        handRows <- fetchAllRows' statement
        let cardIds = (map fromSql (join handRows)) :: [Int]
        return $ Just $ foldr (\thisId hand -> let card = getCard thisId in
                              case hand of [] -> return card
                                           _  ->  card : hand) [] cardIds
        where getCard thisId = (fromJust $ HashMap.lookup thisId idCardMap)

readPlayerHandIds :: (IConnection a) => a -> TableNames -> Int -> IO (Maybe [Int])
readPlayerHandIds conn tableNames whichPlayer = do
        --DISTINCT removes duplicates
        --see https://www.sqlite.org/lang_select.html and http://www.postgresql.org/docs/9.0/static/sql-select.html
        values <- (liftM join) $ (quickQuery' conn ("SELECT DISTINCT whichHand FROM " ++ playerTable) [])
        case values of [] -> return Nothing
                       _  -> return . return . (map fromSql) $ values
        where playerTable = getPlayerTableName tableNames

readPlayerHands :: (IConnection a) => Statement -> a -> Int -> IO (Maybe [Hand])
readPlayerHands statement whichPlayer = undefined
--readPlayerHands statement whichPlayer = do
--        mayHandIds <- readPlayerHandIds conn whichPlayer
--        case mayHandsIds of Nothing -> return Nothing
--                            Just (ids) -> 

getNumPlayers :: (IConnection a) => a -> TableNames -> IO (Int)
getNumPlayers conn tableNames = do
        query <- prepare conn ("SELECT * FROM " ++ playerTable)
        _ <- execute query []
        rows <- fetchAllRows' query
        return . length $ rows
        where playerTable = getPlayerTableName tableNames

readMatchStatement :: (IConnection a) => a -> TableNames -> IO (Statement)
readMatchStatement conn tableNames = prepare conn $ "SELECT dealersHand, whichPlayer, thisPlayersHand, playerResult FROM " ++ matchesTable ++ " WHERE whichGame=?"
            where matchesTable = getMatchTableName tableNames

readMatch :: Statement -> Statement -> Integer -> IO (Maybe Match)
readMatch rMatchStatement rHandStatement whichGame = do
        _ <- execute rMatchStatement [iToSql (fromInteger whichGame)]
        matchRows <- fetchAllRows' rMatchStatement
        case matchRows of [[]] -> return Nothing
                          _ -> extractMatchData rHandStatement matchRows


rowToTuple :: [SqlValue] -> Maybe (Integer, Int, Integer, Int)
rowToTuple thisRow
                | (length thisRow) < 4 = Nothing
                | otherwise = return  . (innerMapTuple2and4 fromIntegral) . (innerMapTuple4 fromSql) $ (thisRow !! 0, thisRow !! 1, thisRow !! 2, thisRow !! 3)
                where innerMapTuple2and4 f (a,b,c,d) = (a, f b, c, f d)

extractMatchData :: Statement -> [[SqlValue]] -> IO (Maybe Match)
extractMatchData rHandStatement rows = do
        --TODO: there must be a more elegant way to check if there's
        --a Nothing in an array and bind it
    let mayCheckedRows = map rowToTuple rows
    if mayCheckedRows == [] || (elem Nothing mayCheckedRows) then return Nothing
                                    else do

        let checkedRows = (convResult (map fromJust mayCheckedRows)) :: [(Integer, Int, Integer, Result)]
        --get the dealer's hand ID
        --it's the same dealer's hand for every game in this match so just get the ID from the first row
        let dHandId = fstIn4 . head $ checkedRows --we already checked that the array isn't empty, so it must have at least 1 array with 1 item
        dHand <- readHand rHandStatement dHandId
        --make sure the dealers hand exists
        if isNothing dHand then return Nothing else do
            (pIds, pHands, pResults) <- liftM unzip3 . sequence $ map (\(_, playerId, playersHandId, playersResult) -> readHand rHandStatement playersHandId >>= 
                                        (\playersReadHand -> return (playerId, playersReadHand, playersResult))) checkedRows

            -- **********************************
            --TODO: rewrite this using bind?
            if elem Nothing pHands then return Nothing
                                            --the fromJust is OK
                                            --because we're checking
                                            --that it isn't Nothing
                                else return . return $ Match (fromJust dHand) pIds (map fromJust pHands) pResults

    where fstIn4 (a,_,_,_) = a
          -- |convert the player result from an integer (its database
          -- representation) back to an enum
          convResult = map (\(a,b,c,d) -> (a,b,c, toEnum d))
