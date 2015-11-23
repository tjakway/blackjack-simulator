module Jakway.Blackjack.IO.Database where

import Database.HDBC

enableForeignKeys :: IConnection a => a -> IO Integer
enableForeignKeys conn = run conn "PRAGMA foreign_keys = ON;" []

flipInner2 :: (a -> b -> c -> d) -> a -> c -> b -> d
flipInner2 f x y z = f x z y

createTables :: IConnection a => a -> IO [Integer]
createTables conn =
            sequence $ map (flipInner2 run conn []) createTableStatements
        where createTableStatements = [ "CREATE TABLE cards (id INTEGER PRIMARY KEY AUTOINCREMENT, cardValue INTEGER NOT NULL, suit INTEGER NOT NULL)",
                                        "CREATE TABLE players (whichPlayer INTEGER PRIMARY KEY)",
                                        "CREATE TABLE hands (id INTEGER PRIMARY KEY AUTOINCREMENT, whichPlayer INTEGER, whichHand INTEGER, thisCard INTEGER, "
                                                            ++ "FOREIGN KEY(whichPlayer) REFERENCES players(whichPlayer), FOREIGN KEY(thisCard) REFERENCES cards(id) )",
                                        "CREATE TABLE matches (id INTEGER PRIMARY KEY AUTOINCREMENT, whichGame INTEGER, dealersHand INTEGER, thisPlayersHand INTEGER, playerResult INTEGER)" ]

insertCardStatement :: (IConnection a) => a -> IO (Statement)
--ignore the id field
insertCardStatement conn = prepare conn "INSERT INTO cards(cardValue, suit) VALUES(?, ?)"
