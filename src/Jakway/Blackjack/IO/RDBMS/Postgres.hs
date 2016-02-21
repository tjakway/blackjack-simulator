--This module should only be accessed by Jakway.Blackjack.IO.DatabaseCommon
--Jakway.Blackjack.IO.DatabaseCommon exports the correct createTables
--function based on flags to `cabal configure`
module Jakway.Blackjack.IO.RDBMS.Postgres where

import Jakway.Blackjack.Util
import Jakway.Blackjack.IO.TableNames
import Database.HDBC


-- ****IMPORTANT****
-- SQL doesn't allow dynamic table
-- names so they have to be manually
-- inserted
-- this is technically a SQL
-- injection vulnerability but it's
-- not like we have any malicious
-- users...


--Postgres uses SERIAL instead of SERIAL
createTables :: IConnection a => a -> TableNames -> IO ()
createTables conn tableNames =
            mapM_ (flipInner2 run conn []) createTableStatements
        where   ptn = getPlayerTableName tableNames  --"player table name"
                htn = getHandTableName tableNames
                mtn = getMatchTableName tableNames
                beginTransac = "BEGIN TRANSACTION"
                commitTransac = "COMMIT TRANSACTION"
                createCardTable = "CREATE TABLE IF NOT EXISTS cards (id SERIAL PRIMARY KEY, cardValue INTEGER NOT NULL, suit INTEGER NOT NULL, visible INTEGER NOT NULL)"
                createPlayersTable = ssub "CREATE TABLE IF NOT EXISTS ?(whichPlayer INTEGER PRIMARY KEY)" [ptn]
                createHandsTable = ssub "CREATE TABLE IF NOT EXISTS ?(id SERIAL PRIMARY KEY, whichPlayer INTEGER REFERENCES ?, whichHand BIGINT NOT NULL, thisCard INTEGER REFERENCES cards(id))" [htn, ptn ++ "(whichPlayer)"]
                createMatchesTable = ssub "CREATE TABLE IF NOT EXISTS ?(id SERIAL PRIMARY KEY, whichGame INTEGER NOT NULL, dealersHand BIGINT NOT NULL, whichPlayer INTEGER REFERENCES ?, thisPlayersHand BIGINT NOT NULL, playerResult INTEGER NOT NULL)" [mtn, htn ++ "(whichHand)", ptn ++ "(whichPlayer)", htn ++ "(whichHand)"]
                createTableStatements = [ beginTransac,
                                          createCardTable,
                                          createPlayersTable,
                                          commitTransac,

                                          beginTransac,
                                          createHandsTable,
                                          createMatchesTable,
                                          commitTransac ]
