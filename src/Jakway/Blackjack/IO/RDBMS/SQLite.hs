--This module should only be accessed by Jakway.Blackjack.IO.DatabaseCommon
--Jakway.Blackjack.IO.DatabaseCommon exports the correct createTables
--function based on flags to `cabal configure`
module Jakway.Blackjack.IO.RDBMS.SQLite where

import Jakway.Blackjack.Util
import Jakway.Blackjack.IO.TableNames
import Database.HDBC

createTables :: IConnection a => a -> TableNames -> IO ()
createTables conn tableNames =
            mapM_ (flipInner2 run conn []) createTableStatements
        where   ptn = getPlayerTableName tableNames  --"player table name"
                htn = getHandTableName tableNames
                mtn = getMatchTableName tableNames
                createCardTable = "CREATE TABLE IF NOT EXISTS cards (id INTEGER PRIMARY KEY AUTOINCREMENT, cardValue INTEGER NOT NULL, suit INTEGER NOT NULL, visible INTEGER NOT NULL)"
                createPlayersTable = ssub "CREATE TABLE IF NOT EXISTS ?(whichPlayer INTEGER PRIMARY KEY AUTOINCREMENT)" [ptn]
                createHandsTable = ssub "CREATE TABLE IF NOT EXISTS ?(id INTEGER PRIMARY KEY AUTOINCREMENT, whichPlayer INTEGER REFERENCES ?, whichHand BIGINT, thisCard INTEGER REFERENCES cards)" [htn, ptn ++ "(whichPlayer)"]
                createMatchesTable = ssub "CREATE TABLE IF NOT EXISTS ?(id INTEGER PRIMARY KEY AUTOINCREMENT, whichGame INTEGER, dealersHand BIGINT REFERENCES ?, whichPlayer INTEGER REFERENCES ?, thisPlayersHand BIGINT REFERENCES ?, playerResult INTEGER)" [mtn, htn ++ "(whichHand)", ptn ++ "(whichPlayer)", htn ++ "(whichHand)"]
                createTableStatements = [ createCardTable,
                                          createPlayersTable,
                                          createHandsTable,
                                          createMatchesTable ]
