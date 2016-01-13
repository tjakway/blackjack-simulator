{-# LANGUAGE DeriveDataTypeable #-}
module Jakway.Blackjack.IO.DatabaseCommon where

import Jakway.Blackjack.Visibility
import Jakway.Blackjack.Cards
import Jakway.Blackjack.CardOps
import Database.HDBC
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as HashMap
import Control.Exception
import Data.Typeable
import Jakway.Blackjack.Util (innerMapTuple4)

type TableNames = (String, String, String, String)

baseTableNames :: TableNames
baseTableNames = ("cards_", "players_", "hands_", "matches_")

getTableNames :: String -> TableNames
getTableNames suffix = innerMapTuple4 (++ suffix) baseTableNames

getCardTableName :: TableNames -> String
getCardTableName (name,_,_,_) = name

getPlayerTableName :: TableNames -> String
getPlayerTableName (_,name,_,_) = name

getHandTableName :: TableNames -> String
getHandTableName (_,_,name,_) = name

getMatchTableName :: TableNames -> String
getMatchTableName (_,_,_,name) = name

-- |reverse operation of cardToForeignId
foreignKeyIdToCard :: Int -> Maybe (Visibility Card)
foreignKeyIdToCard id = HashMap.lookup id idCardMap

-- | XXX: for some reason this function wouldn't work in a where binding?
cardSqlArr :: Suit -> CardValue -> [SqlValue]
cardSqlArr s v = [toSql . fromEnum $ v, toSql . fromEnum $ s]
singleCardToSqlValues :: Visibility Card -> [SqlValue]
singleCardToSqlValues (Shown (Card suit val))   = (cardSqlArr suit val) ++ [iToSql 0]
singleCardToSqlValues (Hidden (Card suit val))  = (cardSqlArr suit val) ++ [iToSql 1]

cardPermutations :: [Visibility Card]
cardPermutations = (Shown <$> newDeck) ++ (Hidden <$> newDeck) 

cardIdMap :: HashMap.Map (Visibility Card) Int
cardIdMap = HashMap.fromList $ zip cardPermutations ids
    where ids = [1..(length cardPermutations)]

-- |TODO: reduce duplication
idCardMap :: HashMap.Map Int (Visibility Card)
idCardMap = HashMap.fromList $ zip ids cardPermutations
    where ids = [1..(length cardPermutations)]


cardToForeignKeyId :: Visibility Card -> Maybe Int
cardToForeignKeyId card = HashMap.lookup card cardIdMap

cardsSqlValues :: [[SqlValue]]
cardsSqlValues = map (\ (id, cardSqlValues) -> (toSql id) : cardSqlValues) (zip ids cardsWithoutIds)
    where cardsWithoutIds = singleCardToSqlValues <$> cardPermutations
          -- |SQL ids count up from 1
          ids = [1..(length cardsWithoutIds)]

--An exception is appropriate for certain cases when reading from the
--database
--Foreign key constraints mean errors *should* never happen, however most
--functions still return Maybe.  If handling the maybe makes an (IO) interface
--unnecessarily complicated it's better to use an exception
data BlackjackDatabaseException = HandReadException
    deriving (Show, Typeable)

instance Exception BlackjackDatabaseException
