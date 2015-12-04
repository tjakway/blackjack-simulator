module Jakway.Blackjack.Tests.DatabaseTests (tests) where

--see http://stackoverflow.com/questions/8502201/remove-file-if-it-exists
import Prelude hiding (catch)
import System.Directory
import Control.Exception
import System.IO.Error hiding (catch)
import Database.HDBC
import Database.HDBC.Sqlite3
import Database.HDBC.Session (withConnectionIO')
import Test.HUnit
import Control.Monad (liftM, when)
import qualified Jakway.Blackjack.IO.Database as DB
import Data.List (sort, delete)
import Test.Framework
import Test.Framework.Providers.HUnit

test_db_name = "tmp_test.db"

--see http://stackoverflow.com/questions/8502201/remove-file-if-it-exists
removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

withDatabase name = withConnectionIO' (connectSqlite3 name)
withTempDatabase transaction dbName = removeIfExists dbName >> withDatabase dbName transaction >> removeIfExists dbName
withTestDatabase transaction = withTempDatabase (\conn -> DB.enableForeignKeys conn >> DB.initializeDatabase conn >> commit conn >> transaction conn) test_db_name


testOpenDatabase :: Assertion
testOpenDatabase = withTestDatabase $ (\_ -> do
                                exists <- doesFileExist test_db_name
                                if exists
                                    then return ()
                                    else assertFailure message)
                    where message = "Database "++test_db_name++" does not exist!"

testTableList :: Assertion
testTableList =  withTestDatabase $ \conn -> getTables conn >>= (\tables -> when (not $ tablesEqual tables) (assertFailure $ message tables))
                where tables = ["cards", "players", "hands", "matches"]
                      -- | in case sqlite adds an extra schema table
                      tablesEqual readTables = (sort tables) == (sort . (delete "sqlite_sequence") $ readTables)
                      message readTables = "Database tables don't match!  Read tables: " ++ (show readTables)

--testInsertPlayers :: Assertion
--testInsertPlayers = withTestDatabase $ \conn -> do
                        

tests =  [testCase "testOpenDatabase" testOpenDatabase, testCase "testTableList" testTableList]
