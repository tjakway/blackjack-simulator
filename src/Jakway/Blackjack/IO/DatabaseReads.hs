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
import Jakway.Blackjack.IO.DatabaseCommon
import Database.HDBC
import qualified Data.Map.Strict as HashMap
import Data.Maybe (fromJust)
import Control.Monad (join, liftM)


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

readPlayerHands :: Statement -> Int -> IO (Maybe [Hand])
readPlayerHands statement whichPlayer = undefined

readMatchStatement :: (IConnection a) => a -> IO (Statement)
readMatchStatement conn = prepare conn "SELECT (dealersHand, whichPlayer, thisPlayersHand, playerResult) FROM matches WHERE whichGame=?"

readMatch :: Statement -> Statement -> Int -> IO (Maybe Match)
readMatch rMatchStatement rHandStatement whichGame = do
        execute rMatchStatement [iToSql whichGame]
        matchRows <- fetchAllRows' rMatchStatement
        case matchRows of [[]] -> return Nothing
                          _ -> do

        where extractResults rows = 


