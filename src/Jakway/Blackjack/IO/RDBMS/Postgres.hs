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
                                        "CREATE TABLE IF NOT EXISTS "++ (getHandTableName tableNames)  ++
                                            "(id SERIAL PRIMARY KEY, whichPlayer INTEGER REFERENCES "++ (getPlayerTableName tableNames) ++ ", whichHand BIGINT REFERENCES "++ (getHandTableName tableNames)++", thisCard INTEGER REFERENCES cards, ",

                                        "CREATE TABLE IF NOT EXISTS "++ (getMatchTableName tableNames) ++
                                            "(id SERIAL PRIMARY KEY, whichGame INTEGER REFERENCES, dealersHand BIGINT REFERENCES "++ (getHandTableName tableNames) ++", whichPlayer INTEGER REFERENCES "++ (getPlayerTableName tableNames) ++ ", thisPlayersHand BIGINT REFERENCES " ++ (getHandTableName tableNames) ++ ", playerResult INTEGER, " ++
                                                              "FOREIGN KEY(dealersHand) REFERENCES "++ (getHandTableName tableNames) ++"(whichHand), FOREIGN KEY(whichPlayer) REFERENCES "++ (getPlayerTableName tableNames) ++"(whichPlayer), FOREIGN KEY(thisPlayersHand) REFERENCES "++ (getHandTableName tableNames) ++"(whichHand) ) ",
                                                             "COMMIT TRANSACTION" ]
