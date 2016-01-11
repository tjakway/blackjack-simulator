module Jakway.Blackjack.IO.DatabaseReads 
(readPlayers,
 readHandStatement,
 readHand,
 readPlayerHands,
 readMatchStatement,
 readMatch 
)
where

import Prelude hiding (lookup)
import Jakway.Blackjack.Visibility
import Jakway.Blackjack.Cards
import Jakway.Blackjack.CardOps
import Jakway.Blackjack.Match
import Jakway.Blackjack.IO.DatabaseCommon
import Database.HDBC
import qualified Data.Map.Strict as HashMap
import Data.Maybe (fromJust)
import Control.Monad (join, liftM, when)
import Data.List (unzip3)
import Control.Exception

readPlayers :: (IConnection a) => a -> IO ([Int])
readPlayers conn = do
        values <- quickQuery' conn "SELECT whichPlayer FROM players" []
        -- |quickQuery' returns [[SqlValue]].  Collapse the list.
        (return . (map fromSql)) (join values)

readHandStatement :: (IConnection a) => a -> IO (Statement)
readHandStatement conn = prepare conn "SELECT thisCard FROM hands WHERE whichHand=?"

readHand :: Statement -> Integer -> IO (Maybe Hand)
readHand statement whichHand = do
        execute statement [toSql whichHand]
        handRows <- fetchAllRows' statement
        let cardIds = (map fromSql (join handRows)) :: [Int]
        return $ Just $ foldr (\thisId hand -> let card = getCard thisId in
                              case hand of [] -> return card
                                           _  ->  card : hand) [] cardIds
        where getCard thisId = (fromJust $ HashMap.lookup thisId idCardMap)

readPlayerHandIds :: (IConnection a) => a -> Int -> IO (Maybe [Int])
readPlayerHandIds conn whichPlayer = do
        --DISTINCT removes duplicates
        --see https://www.sqlite.org/lang_select.html and http://www.postgresql.org/docs/9.0/static/sql-select.html
        values <- (liftM join) $ (quickQuery' conn "SELECT DISTINCT whichHand" [])
        case values of [] -> return Nothing
                       _  -> return . return . (map fromSql) $ values

readPlayerHands :: (IConnection a) => Statement -> a -> Int -> IO (Maybe [Hand])
readPlayerHands statement whichPlayer = undefined
--readPlayerHands statement whichPlayer = do
--        mayHandIds <- readPlayerHandIds conn whichPlayer
--        case mayHandsIds of Nothing -> return Nothing
--                            Just (ids) -> 

readMatchStatement :: (IConnection a) => a -> IO (Statement)
readMatchStatement conn = prepare conn "SELECT (dealersHand, whichPlayer, thisPlayersHand, playerResult) FROM matches WHERE whichGame=?"

mapTuple4 f (a,b,c,d) = (f a, f b, f c, f d)

readMatch :: Statement -> Statement -> Int -> IO (Maybe Match)
readMatch rMatchStatement rHandStatement whichGame = do
        execute rMatchStatement [iToSql whichGame]
        matchRows <- fetchAllRows' rMatchStatement
        case matchRows of [[]] -> return Nothing
                          _ -> extractResults rHandStatement matchRows


rowToTuple :: [SqlValue] -> Maybe (a,b,c,d)
rowToTuple thisRow
                | (length thisRow) < 4 = Nothing
                | otherwise = return $ mapTuple4 fromSql (thisRow !! 0, thisRow !! 1, thisRow !! 2, thisRow !! 3)





extractResults :: Statement -> [[SqlValue]] -> IO (Maybe Match)
extractResults rHandStatement rows = do
    let mayCheckedRows = map rowToTuple rows
    if elem Nothing mayCheckedRows then return Nothing
                                    else do

        let checkedRows = map fromJust mayCheckedRows
        --get the dealer's hand ID
        --it's the same dealer's hand for every game in this match so just get the ID from the first row
        let dHandId = fromSql ((rows !! 0) !! 0) --we already checked that the array isn't empty, so it must have at least 1 array with 1 item
        dHand <- readHand rHandStatement dHandId
        --make sure the dealers hand exists
        if dHand == Nothing then return Nothing else do
            (pIds, pHands, pResults) <- unzip3 $ map (\(_, playerId, playersHandId, playersResult) -> readHand rHandStatement (fromSql playersHandId) >>= 
                                        (\playersReadHand -> if playersReadHand == Nothing then throw HandReadException else return (fromSql playerId, fromJust playersReadHand, fromSql playersResult))) checkedRows

            -- **********************************
            --TODO: rewrite this using bind?
            if elem Nothing pHands then return Nothing
                                            --the fromJust is OK
                                            --because we're checking
                                            --that it isn't Nothing
                                else return $ Match (fromJust dHand) pIds (map fromJust pHands) pResults
