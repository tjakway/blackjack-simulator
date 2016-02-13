{-# LANGUAGE CPP #-}
module Jakway.Blackjack.IO.DatabaseConnection where

import Control.Exception

#ifdef BUILD_POSTGRESQL
postgres_conf_filename :: FilePath
postgres_conf_filename = "psql_conn.conf"

default_connection_string :: String
default_connection_string = ""

mayReadFile :: FilePath -> IO (Maybe String)
mayReadFile path = do
    result <- (try (evaluate (readFile path))) :: IO (Either IOException (IO String))
    case result of
        Left _ -> return Nothing
        --contents has a type of IO String, need to convert it to an 
        --IO (Maybe String)
        Right contents -> contents >>= \c -> return . Just $ c

readPostgresConnectionString :: IO (String)
readPostgresConnectionString = do
        confFile <- mayReadFile postgres_conf_filename
        case confFile of
            Nothing -> return ""
            Just contents -> return contents
#else
--SQLite

#endif
