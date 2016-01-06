module Jakway.Blackjack.IO.DatabaseReads where

import Prelude hiding (lookup)
import Jakway.Blackjack.Visibility
import Jakway.Blackjack.Cards
import Jakway.Blackjack.CardOps
import Jakway.Blackjack.IO.DatabaseCommon
import Database.HDBC
import qualified Data.Map.Strict as HashMap
import Data.Maybe (fromJust)
import Control.Monad (join)


readPlayers :: (IConnection a) => a -> IO ([Int])
readPlayers conn = do
        values <- quickQuery' conn "SELECT whichPlayer FROM players" []
        -- |quickQuery' returns [[SqlValue]].  Collapse the list.
        let playerInts = map fromSql (join values)
        return playerInts

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


readPlayerStatement :: (IConnection a) => a -> IO (Statement)
readPlayerStatement conn = prepare conn "SELECT whichPlayer FROM players"

readPlayers :: Statement -> IO ([Int])
readPlayers statement = do
        execute statement []
        --could this be replaced with a single line
        --return . join . fetchAllRows' $ statement  ?
        playerRows <- join $ fetchAllRows' statement
        return playerRows

readPlayerHands :: Statement -> Int -> IO (Maybe [Hand])
readPlayerHands statement whichPlayer = undefined

readMatchStatement :: (IConnection a) => a -> IO (Statement)
readMatchStatement conn = undefined
