--This module should only be accessed by Jakway.Blackjack.IO.DatabaseCommon
--Jakway.Blackjack.IO.DatabaseCommon exports the correct createTables
--function based on flags to `cabal configure`
module Jakway.Blackjack.IO.RDBMS.Postgres where

import Jakway.Blackjack.Util
import Jakway.Blackjack.IO.TableNames
import Database.HDBC

--Postgres uses SERIAL instead of SERIAL
createTables :: IConnection a => a -> TableNames -> IO ()
createTables conn tableNames =
            mapM_ (flipInner2 run conn []) createTableStatements
        where createTableStatements = [ "BEGIN TRANSACTION",
                                      "CREATE TABLE IF NOT EXISTS cards (id SERIAL PRIMARY KEY, cardValue INTEGER NOT NULL, suit INTEGER NOT NULL, visible INTEGER NOT NULL)",
                                      -- ^ Sqlite doesn't have a boolean
                                      -- data type, see https://www.sqlite.org/datatype3.html and http://stackoverflow.com/questions/843780/store-boolean-value-in-sqlite
                                      -- ****IMPORTANT****
                                      -- SQL doesn't allow dynamic table
                                      -- names so they have to be manually
                                      -- inserted
                                      -- this is technically a SQL
                                      -- injection vulnerability but it's
                                      -- not like we have any malicious
                                      -- users...
                                        "CREATE TABLE IF NOT EXISTS "++ (getPlayerTableName tableNames) ++" (whichPlayer INTEGER PRIMARY KEY)",
                                        "COMMIT TRANSACTION",
                                        "BEGIN TRANSACTION",
                                        -- | Note that SQLite doesn't allow
                                        -- any datatype other than INTEGER
                                        -- to be declared PRIMARY KEY
                                        -- SERIAL
                                        "CREATE TABLE IF NOT EXISTS "++ (getHandTableName tableNames)  ++" (id SERIAL PRIMARY KEY, whichPlayer INTEGER, whichHand BIGINT, thisCard INTEGER, "
                                                            ++ "FOREIGN KEY(whichPlayer) REFERENCES players(whichPlayer), FOREIGN KEY(thisCard) REFERENCES cards(id) )",
                                        "CREATE TABLE IF NOT EXISTS "++ (getMatchTableName tableNames) ++"(id SERIAL PRIMARY KEY, whichGame INTEGER, dealersHand BIGINT, whichPlayer INTEGER, thisPlayersHand BIGINT, playerResult INTEGER, " ++
                                                              "FOREIGN KEY(dealersHand) REFERENCES hands(whichHand), FOREIGN KEY(whichPlayer) REFERENCES players(whichPlayer), FOREIGN KEY(thisPlayersHand) REFERENCES hands(whichHand) ) ",
                                                             "COMMIT TRANSACTION" ]
