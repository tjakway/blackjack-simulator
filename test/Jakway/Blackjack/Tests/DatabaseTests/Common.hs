{-# LANGUAGE CPP #-}
module Jakway.Blackjack.Tests.DatabaseTests.Common where

--see http://stackoverflow.com/questions/8502201/remove-file-if-it-exists
--for why hiding catch is a good idea
import Prelude hiding (catch)
import Control.Exception
import System.IO.Error hiding (catch)
import System.Directory
import Database.HDBC
import Database.HDBC.Sqlite3
import Database.HDBC.Session (withConnectionIO')
import qualified Jakway.Blackjack.IO.DatabaseWrites as DB
import qualified Jakway.Blackjack.IO.DatabaseReads as DB
import qualified Jakway.Blackjack.IO.DatabaseCommon as DB
import qualified Jakway.Blackjack.IO.DatabaseConnection as DBConn
import qualified Jakway.Blackjack.IO.TableNames as DB

#ifdef BUILD_POSTGRESQL
withDatabase name = withConnectionIO' $ DBConn.connectPostgresDBReadString
#else
test_db_name = "test_tmp.db"
withDatabase name = withConnectionIO' $ DBConn.connectSQLiteDB test_db_name
#endif

--see http://stackoverflow.com/questions/8502201/remove-file-if-it-exists
removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e


-- |run a transaction on a database that will be deleted before and after running it
withTempDatabase dbName transaction = removeIfExists dbName >> withDatabase dbName transaction >> removeIfExists dbName

basicTestTableNames = DB.getTableNames "test1"

-- |initialize the database then run the transaction
-- don't forget to commit!
withSingleTableTestDatabase transaction = withTempDatabase test_db_name (\conn -> DB.enableForeignKeys conn >> DB.initializeDatabase conn [basicTestTableNames] >> DB.insertAllCards conn >> commit conn >> (handleSqlError $ transaction conn))
