{-# LANGUAGE CPP #-}
module Jakway.Blackjack.Tests.DatabaseTests.Common where

--see http://stackoverflow.com/questions/8502201/remove-file-if-it-exists
--for why hiding catch is a good idea
import Prelude hiding (catch)
import Control.Exception
import System.IO.Error hiding (catch)
import System.Directory
import Database.HDBC
import Database.HDBC.Session (withConnectionIO')
import qualified Jakway.Blackjack.IO.DatabaseWrites as DB
import qualified Jakway.Blackjack.IO.DatabaseReads as DB
import qualified Jakway.Blackjack.IO.DatabaseCommon as DB
import qualified Jakway.Blackjack.IO.DatabaseConnection as DBConn
import qualified Jakway.Blackjack.IO.TableNames as DB
import Jakway.Blackjack.Tests.Constants (test_db_name)
#ifdef BUILD_POSTGRESQL
import Database.HDBC.PostgreSQL (Connection)
#else
import Database.HDBC.Sqlite3 (Connection)
#endif

withDatabase :: FilePath -> (Connection -> IO b) -> IO b
#ifdef BUILD_POSTGRESQL
withDatabase _ = withConnectionIO' DBConn.connectPostgresDBReadString
#else
withDatabase name = withConnectionIO' $ DBConn.connectSQLiteDB test_db_name
#endif

--see http://stackoverflow.com/questions/8502201/remove-file-if-it-exists
removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e


withTempDatabase :: FilePath -> DB.TableNames -> (Connection -> IO b) -> IO ()
#ifdef BUILD_POSTGRESQL
--database name is irrelevent if we're using postgres
--need the table names to wipe the postgres database
withTempDatabase dbName tableNames transaction = withDatabase dbName trans_and_drop
            where trans_and_drop = (\conn -> DB.dropAllTables conn >> commit conn >> transaction conn >> commit conn >> DB.dropAllTables conn >> commit conn)
#else
-- |run a transaction on a database that will be deleted before and after running it
withTempDatabase dbName tableNames transaction = removeIfExists dbName >> withDatabase dbName transaction >> removeIfExists dbName
#endif

basicTestTableNames = DB.getTableNames "test1"


-- |initialize the database then run the transaction
-- don't forget to commit!
withSingleTableTestDatabase transaction = withTempDatabase test_db_name basicTestTableNames (\conn -> DB.enableForeignKeys conn >> DB.initializeDatabase conn [basicTestTableNames] >> DB.insertAllCards conn >> commit conn >> (handleSqlError $ transaction conn))
