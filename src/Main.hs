{-# LANGUAGE ScopedTypeVariables, CPP #-}
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
import Jakway.Blackjack.Interface.Options


main :: IO ()
main = do
        args <- getArgs
        (beVerbose, dealerAI, playerAIs, numGames, suffix) <- getConfig args
        when (beVerbose == True) $ do
            putStrLn "Using options: "
            putStrLn $ "verbose: " ++ (show beVerbose)
            putStrLn $ "Dealer AI:" ++ (show dealerAI)
            putStrLn $ "Player AIs: " ++ (show playerAIs)
            putStrLn $ "Number of games: " ++ (show numGames)
        
          --get the hand and match insert statements
    where getStatements :: (IConnection a) => a -> TableNames -> IO (Statement, Statement)
          getStatements conn names = insertHandStatement conn names >>= 
                                        (\ihs -> insertMatchStatement conn names >>= 
                                            (\ims -> return (ihs, ims)))
          db_spec_main :: Config -> IO ()
#ifdef BUILD_POSTGRESQL          
          db_spec_main config (beVerbose, dealerAI, playerAIs, numGames) = do
              --TODO: extract table names from config
              conn_string <- readPostgresConnectionString
              when (beVerbose == True) $ putStrLn $ "Using Postgres connection string: " ++ conn_string
              conn <- connectPostgresDB conn_string

              (insHandStatement, insMatchStatement) <- getStatements conn tableNames

              foldr (\_ -> ) [0..numGames]
#else
          db_spec_main = undefined
#endif
