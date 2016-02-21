module Main where

import Control.Monad (liftM, when)
import System.Environment (getArgs)
import Jakway.Blackjack.AI
import Jakway.Blackjack.Game
import Jakway.Blackjack.Cards
import Jakway.Blackjack.CardOps
import Jakway.Blackjack.Result
import Jakway.Blackjack.Visibility
import Jakway.Blackjack.IO.DatabaseWrites
import Jakway.Blackjack.IO.DatabaseReads
import Jakway.Blackjack.Interface.Options

main :: IO ()
main = do
        args <- getArgs
        (beVerbose, dealerAI, playerAIs, numGames) <- getConfig args
        when (beVerbose == True) $ do
            putStrLn "Using options: "
            putStrLn $ "verbose: " ++ (show beVerbose)
            putStrLn $ "Dealer AI:" ++ (show dealerAI)
            putStrLn $ "Player AIs: " ++ (show playerAIs)
            putStrLn $ "Number of games: " ++ (show numGames)
