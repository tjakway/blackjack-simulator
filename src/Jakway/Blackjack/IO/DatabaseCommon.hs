{-# LANGUAGE DeriveDataTypeable, CPP #-}
module Jakway.Blackjack.IO.DatabaseCommon
(
createTables,
cardIdMap,
cardPermutations,
cardSqlArr,
cardsSqlValues,
cardToForeignKeyId,
idCardMap,
singleCardToSqlValues,
dropTables,
dropAllTables
)
where

import Jakway.Blackjack.Visibility
import Jakway.Blackjack.Cards
import Jakway.Blackjack.CardOps
import Database.HDBC
import qualified Data.Map.Strict as HashMap
import Control.Exception
import Data.Typeable
import Jakway.Blackjack.Util
import Jakway.Blackjack.IO.TableNames
import Jakway.Blackjack.IO.DatabaseConnection
import qualified Jakway.Blackjack.IO.RDBMS.Postgres as Postgres
import qualified Jakway.Blackjack.IO.RDBMS.SQLite as SQLite
import Control.Monad (join)

#ifdef BUILD_POSTGRESQL
createTables :: IConnection a => a -> TableNames -> IO ()
createTables = Postgres.createTables
#else
createTables :: IConnection a => a -> TableNames -> IO ()
createTables = SQLite.createTables
#endif

dropAllTables :: IConnection a => a -> IO()
dropAllTables conn = withTransaction conn $ \t_conn -> getDropStatement t_conn >>= (\dropStatement -> 
                        getTableSQLValues t_conn >>= executeMany dropStatement )
        where getTableSQLValues p_conn = getTables p_conn >>= (\t -> return $ map (\x -> [toSql x]) t)
              --remove newlines
              getDropStatement p_conn = prepare p_conn dropStr
              --see http://stackoverflow.com/questions/10050988/haskell-removes-all-occurrences-of-a-given-value-from-within-a-list-of-lists 
              dropStr = filter (/= '\n') $ "DROP TABLE IF EXISTS ? " ++ cascadeStr 
              cascadeStr = 
              --cascade so we don't cause errors with foreign keys
#ifdef BUILD_POSTGRESQL
                            "CASCADE"
#else
                            ""
#endif

dropTables :: IConnection a => a -> TableNames -> IO ()
dropTables conn tableNames = 
            mapM_ (flipInner2 run conn []) dropTableStatements >> commit conn
--CASCADE is postgres-specific
#ifdef BUILD_POSTGRESQL
        where dropTableStatements = [ "BEGIN TRANSACTION",
                                    "DROP TABLE IF EXISTS cards CASCADE",
                                    "DROP TABLE IF EXISTS " ++ (getPlayerTableName tableNames) ++ " CASCADE",
                                    "DROP TABLE IF EXISTS " ++ (getHandTableName tableNames) ++ " CASCADE",
                                    "DROP TABLE IF EXISTS " ++ (getMatchTableName tableNames) ++ " CASCADE",
                                   "COMMIT TRANSACTION" ]
#else
        where dropTableStatements = [ "DROP TABLE IF EXISTS cards",
                                    "DROP TABLE IF EXISTS " ++ (getPlayerTableName tableNames),
                                    "DROP TABLE IF EXISTS " ++ (getHandTableName tableNames),
                                    "DROP TABLE IF EXISTS " ++ (getMatchTableName tableNames) ]
#endif

-- |reverse operation of cardToForeignId
foreignKeyIdToCard :: Int -> Maybe (Visibility Card)
foreignKeyIdToCard pId = HashMap.lookup pId idCardMap

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
cardsSqlValues = map (\ (cIds, cardSqlValues) -> (toSql cIds) : cardSqlValues) (zip pIds cardsWithoutIds)
    where cardsWithoutIds = singleCardToSqlValues <$> cardPermutations
          -- |SQL ids count up from 1
          pIds = [1..(length cardsWithoutIds)]

--An exception is appropriate for certain cases when reading from the
--database
--Foreign key constraints mean errors *should* never happen, however most
--functions still return Maybe.  If handling the maybe makes an (IO) interface
--unnecessarily complicated it's better to use an exception
data BlackjackDatabaseException = HandReadException
    deriving (Show, Typeable)

instance Exception BlackjackDatabaseException
