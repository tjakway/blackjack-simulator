module Jakway.Blackjack.IO.Database where

import Jakway.Blackjack.Cards
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

cardToSqlValues :: Card -> [SqlValue]
cardToSqlValues (Card val suit) = [toSql . fromEnum $ val, toSql . fromEnum $ suit]

insertAllCards :: (IConnection a) => a -> IO ()
insertAllCards conn = do 
                         let cardSqlValues = map cardToSqlValues newDeck 
                             -- ^ newDeck is a (sorted) array of all possible card values
                         insertStatement <- insertCardStatement conn
                         executeMany insertStatement cardSqlValues

insertPlayerStatement :: (IConnection a) => a -> IO (Statement)
insertPlayerStatement conn = prepare conn "INSERT INTO players(whichPlayer) VALUES(?)"

insertPlayers :: (IConnection a) => a -> Int -> IO ()
insertPlayers conn numPlayers = insertPlayerStatement conn >>= (\insertStatement -> executeMany insertStatement insValues)
                        where insValues = map ((: []) . toSql) [0..numPlayers]
                            -- ^ player number 0 is the dealer!

insertHandStatement :: (IConnection a) => a -> IO (Statement)
insertHandStatement conn = undefined


