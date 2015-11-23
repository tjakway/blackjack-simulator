module Jakway.Blackjack.IO.Database where

import Database.HDBC

createDB :: IConnection a => a -> IO [()]
createDB conn =
        sequence $ map (runRaw conn) createTableStatements
        where createTableStatements = [ "CREATE TABLE cards (id INTEGER PRIMARY KEY AUTOINCREMENT, cardValue INTEGER NOT NULL, suit INTEGER NOT NULL)",
                                        "CREATE TABLE players (whichPlayer INTEGER PRIMARY KEY)",
                                        "CREATE TABLE hands (id INTEGER PRIMARY KEY AUTOINCREMENT, whichPlayers INTEGER, whichHand INTEGER, thisCard INTEGER, "
                                                            ++ "FOREIGN KEY(whichPlayer) REFERENCES players(whichPlayer), FOREIGN KEY(thisCard) REFERENCES cards(id) )",
                                        "CREATE TABLE matches (id INTEGER PRIMARY KEY AUTOINCREMENT, whichGame INTEGER, dealersHand INTEGER)" ]
