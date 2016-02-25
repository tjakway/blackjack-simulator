{-# LANGUAGE CPP #-}
module Main where

import Control.Monad (liftM, when, mapM_)
import System.Environment (getArgs)
import Jakway.Blackjack.AI
import Jakway.Blackjack.Game
import Jakway.Blackjack.Cards
import Jakway.Blackjack.CardOps
import Jakway.Blackjack.Result
import Jakway.Blackjack.Visibility
import Jakway.Blackjack.IO.DatabaseWrites
import Jakway.Blackjack.IO.DatabaseReads
import Jakway.Blackjack.IO.Action
import Jakway.Blackjack.IO.DatabaseConnection
import Jakway.Blackjack.IO.TableNames
import Jakway.Blackjack.Interface.Options
import Database.HDBC
import System.Random

matches_per_transaction = 1000


main :: IO ()
main = do
        args <- getArgs
        conf <- getConfig args
        let (beVerbose, dealerAI, playerAIs, numGames, suffix) = conf
        when (beVerbose == True) $ print_verbose conf

        where db_spec_main :: Config -> IO ()
#ifdef BUILD_POSTGRESQL          
          db_spec_main conf = do
              let (beVerbose, dealerAI, playerAIs, numGames, suffix) = conf
              let tableNames = getTableNames suffix

              --connect to the database
              conn_string <- readPostgresConnectionString
              when (beVerbose == True) $ putStrLn $ "Using Postgres connection string: " ++ conn_string
              conn <- connectPostgresDB conn_string


              --prepare the database
              insertPlayers conn tableNames dealerAI playerAIs

#else
          db_spec_main = undefined
#endif


print_verbose :: Config -> IO ()
print_verbose conf = do
        let (beVerbose, dealerAI, playerAIs, numGames, suffix) = conf
        putStrLn "Using options: "
        putStrLn $ "verbose: " ++ (show beVerbose)
        putStrLn $ "Dealer AI:" ++ (show dealerAI)
        putStrLn $ "Player AIs: " ++ (show playerAIs)
        putStrLn $ "Number of games: " ++ (show numGames)
