{-# LANGUAGE CPP #-}
module Jakway.Blackjack.IO.DatabaseConnection 
(
#ifdef BUILD_POSTGRESQL
connectPostgresDB,
connectPostgresDBReadString,
#else
connectSQLiteDB
#endif
)
where

import Database.HDBC
import Control.Exception

#ifdef BUILD_POSTGRESQL
import Database.HDBC.PostgreSQL
import System.Directory (doesFileExist)

postgres_conf_filename :: FilePath
postgres_conf_filename = "psql_conn.conf"

default_connection_string :: String
default_connection_string = ""

mayReadFile :: FilePath -> IO (Maybe String)
mayReadFile path = do
    exists <- doesFileExist path
    if exists == False then return Nothing
                       else do
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

connectPostgresDB :: String -> IO Connection
connectPostgresDB connStr = connectPostgreSQL' connStr

--connect to the database using the options read from the file
connectPostgresDBReadString :: IO Connection
connectPostgresDBReadString = readPostgresConnectionString >>= connectPostgresDB

#else

import Database.HDBC.Sqlite3

--SQLite
connectSQLiteDB :: FilePath -> IO Connection
connectSQLiteDB = connectSqlite3

#endif
