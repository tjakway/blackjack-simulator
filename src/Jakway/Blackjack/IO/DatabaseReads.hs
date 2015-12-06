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

readHand :: Statement -> Int -> IO (Maybe Hand)
readHand statement whichHand = do
        execute statement [toSql whichHand]
        handRows <- fetchAllRows' statement
        let cardIds = (map fromSql (join handRows)) :: [Int]
        return $ Just $ foldr (\thisId hand -> (fromJust $ HashMap.lookup thisId idCardMap) : hand) [] cardIds


readPlayerStatement :: (IConnection a) => a -> IO (Statement)
readPlayerStatement conn = undefined

readPlayerHands :: Statement -> Int -> IO (Maybe [Hand])
readPlayerHands statement whichPlayer = undefined
