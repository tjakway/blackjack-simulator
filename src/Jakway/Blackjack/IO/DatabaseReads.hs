module Jakway.Blackjack.IO.DatabaseReads where

import Jakway.Blackjack.Visibility
import Jakway.Blackjack.Cards
import Jakway.Blackjack.CardOps
import Database.HDBC
import qualified Data.Map.Strict as HashMap
import Control.Monad (join)

readPlayers :: (IConnection a) => a -> IO ([Int])
readPlayers conn = do
        values <- quickQuery' conn "SELECT whichPlayer FROM players" []
        -- |quickQuery' returns [[SqlValue]].  Collapse the list.
        let playerInts = map fromSql (join values)
        return playerInts

readHandStatement :: (IConnection a) => a -> IO (Statement)
readHandStatement conn = undefined

readHand :: Statement -> Int -> IO (Maybe Hand)
readHand statement whichHand = undefined

readPlayerStatement :: (IConnection a) => a -> IO (Statement)
readPlayerStatement conn = undefined

readPlayerHands :: Statement -> Int -> IO (Maybe [Hand])
readPlayerHands statement whichPlayer = undefined
