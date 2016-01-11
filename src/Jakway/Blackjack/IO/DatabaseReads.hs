{-# LANGUAGE FlexibleContexts #-}
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
import Jakway.Blackjack.Result
import Database.HDBC
import qualified Data.Map.Strict as HashMap
import Data.Maybe (fromJust)
import Control.Monad (join, liftM, when)
import Data.List (unzip3)
import Control.Exception
import Data.Convertible

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

innerMapTuple4 f (a,b,c,d) = (f a, f b, f c, f d)

readMatch :: Statement -> Statement -> Int -> IO (Maybe Match)
readMatch rMatchStatement rHandStatement whichGame = do
        execute rMatchStatement [iToSql whichGame]
        matchRows <- fetchAllRows' rMatchStatement
        case matchRows of [[]] -> return Nothing
                          _ -> extractMatchData rHandStatement matchRows


rowToTuple :: (Convertible SqlValue a) => [SqlValue] -> Maybe (a,a,a,a)
rowToTuple thisRow
                | (length thisRow) < 4 = Nothing
                | otherwise = return $ innerMapTuple4 fromSql (thisRow !! 0, thisRow !! 1, thisRow !! 2, thisRow !! 3)





extractMatchData :: Statement -> [[SqlValue]] -> IO (Maybe Match)
extractMatchData rHandStatement rows = do
        --TODO: there must be a more elegant way to check if there's
        --a Nothing in an array and bind it
    let mayCheckedRows = map rowToTuple rows
    if elem Nothing mayCheckedRows then return Nothing
                                    else do

        let checkedRows = (convResult (map fromJust mayCheckedRows)) :: [(Integer, Int, Integer, Result)]
        --get the dealer's hand ID
        --it's the same dealer's hand for every game in this match so just get the ID from the first row
        let dHandId = fstIn4 . head $ checkedRows --we already checked that the array isn't empty, so it must have at least 1 array with 1 item
        dHand <- readHand rHandStatement dHandId
        --make sure the dealers hand exists
        if dHand == Nothing then return Nothing else do
            (pIds, pHands, pResults) <- unzip3 $ sequence $ map (\(_, playerId, playersHandId, playersResult) -> readHand rHandStatement playersHandId >>= 
                                        (\playersReadHand -> if playersReadHand == Nothing then throw HandReadException else return ((playerId) :: Int, (fromJust playersReadHand) :: Hand, (playersResult) :: Result))) checkedRows

            -- **********************************
            --TODO: rewrite this using bind?
            if elem Nothing pHands then return Nothing
                                            --the fromJust is OK
                                            --because we're checking
                                            --that it isn't Nothing
                                else return . return $ Match (fromJust dHand) pIds pHands pResults

    where fstIn4 (a,_,_,_) = a
          -- |convert the player result from an integer (its database
          -- representation) back to an enum
          convResult = map (\(a,b,c,d) -> (a,b,c, toEnum d))
