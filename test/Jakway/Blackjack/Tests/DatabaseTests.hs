module Jakway.Blackjack.Tests.DatabaseTests where

--see http://stackoverflow.com/questions/8502201/remove-file-if-it-exists
import Prelude hiding (catch)
import System.Directory
import Control.Exception
import System.IO.Error hiding (catch)
import Database.HDBC
import Database.HDBC.Sqlite3
import Test.HUnit
import Control.Monad (liftM)
import qualified Jakway.Blackjack.IO.Database as DB

test_db_name = "tmp_test.db"

--see http://stackoverflow.com/questions/8502201/remove-file-if-it-exists
removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

withDatabase dbName = let openConnection = connectSqlite3 dbName
                    in openConnection >>= (\conn -> bracket (return (conn)) acquireDB releaseDB)
                    where acquireDB openConnection = openConnection >>= (\conn -> DB.enableForeignKeys conn >> DB.createTables conn >> commit conn)
                          releaseDB openConnection = openConnection >>= (\conn -> commit conn >> disconnect conn >> removeIfExists dbName)

withTestDatabase = withDatabase test_db_name

testOpenDatabase = TestCase (assertBool ("Make sure database"++test_db_name++" exists") True (withTestDatabase >> doesFileExist test_db_name))

tests = TestList [TestLabel "testOpenDatabase" testOpenDatabase]
