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

--see http://stackoverflow.com/questions/8502201/remove-file-if-it-exists
removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

withDatabase name = withConnectionIO' (connectSqlite3 name) 

-- |run a transaction on a database that will be deleted before and after running it
withTempDatabase dbName transaction = removeIfExists dbName >> withDatabase dbName transaction >> removeIfExists dbName

test_db_name = "tmp_test.db"

basicTestTableNames = DB.getTableNames "test1"

-- |initialize the database then run the transaction
-- don't forget to commit!
withSingleTableTestDatabase transaction = withTempDatabase test_db_name (\conn -> DB.enableForeignKeys conn >> DB.initializeDatabase conn [basicTestTableNames] >> DB.insertAllCards conn >> commit conn >> (handleSqlError $ transaction conn))
