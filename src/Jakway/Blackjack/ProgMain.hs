{-# LANGUAGE CPP #-}

module Jakway.Blackjack.ProgMain 
(
progMain
)
where

import Control.Monad (liftM, when, mapM_, unless)
import System.Environment (getArgs)
import Jakway.Blackjack.AI
import Jakway.Blackjack.Game
import Jakway.Blackjack.Cards
import Jakway.Blackjack.CardOps
import Jakway.Blackjack.Result
import Jakway.Blackjack.Visibility
import Jakway.Blackjack.IO.DatabaseWrites
import Jakway.Blackjack.IO.DatabaseCommon
import Jakway.Blackjack.IO.DatabaseReads
import Jakway.Blackjack.IO.Action
import Jakway.Blackjack.IO.DatabaseConnection
import Jakway.Blackjack.IO.TableNames
import Jakway.Blackjack.Interface.Options
import Database.HDBC
import System.Random

matches_per_transaction = 10


progMain :: IO ()
progMain = do
        args <- getArgs
        conf <- getConfig args
        let (beVerbose, dealerAI, playerAIs, numGames, suffix) = conf
        when (beVerbose == True) $ printOptions conf

        db_spec_main conf

        where db_spec_main :: Config -> IO ()
#ifdef BUILD_POSTGRESQL          
              db_spec_main conf = handleSqlError $ do
                let (beVerbose, dealerAI, playerAIs, numGames, suffix) = conf
                let tableNames = getTableNames suffix

                --connect to the database
                conn_string <- readPostgresConnectionString
                when (beVerbose == True) $ putStrLn $ "Using Postgres connection string: " ++ conn_string
                conn <- connectPostgresDB conn_string

                initializeDatabase conn [tableNames]
                createTables conn tableNames
                commit conn
                db_tables <- getTables conn
                commit conn
                
                --only insert cards if the table is empty
                read_cards <- quickQuery' conn "SELECT * FROM cards" []
                when (read_cards == []) (withTransaction conn insertAllCards)

                commit conn
                --prepare the database
                insertPlayers conn tableNames dealerAI playerAIs
                commit conn

                res <- collapseMatches matches_per_transaction conf conn 
                printResults res

#else
              db_spec_main = undefined
#endif

printResults :: Either String Integer -> IO ()
printResults (Left message) = putStrLn $ "ERROR.  message: " ++ message
printResults (Right _) = putStrLn "Operations successful."

printOptions :: Config -> IO ()
printOptions conf = do
        let (beVerbose, dealerAI, playerAIs, numGames, suffix) = conf
        putStrLn "Using options: "
        putStrLn $ "verbose: " ++ (show beVerbose)
        putStrLn $ "Dealer AI:" ++ (show dealerAI)
        putStrLn $ "Player AIs: " ++ (show playerAIs)
        putStrLn $ "Number of games: " ++ (show numGames)
