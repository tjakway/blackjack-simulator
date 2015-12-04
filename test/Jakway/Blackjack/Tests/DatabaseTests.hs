module Jakway.Blackjack.Tests.DatabaseTests (tests) where

--see http://stackoverflow.com/questions/8502201/remove-file-if-it-exists
import Prelude hiding (catch)
import System.Directory
import Control.Exception
import System.IO.Error hiding (catch)
import Database.HDBC
import Database.HDBC.Sqlite3
import Database.HDBC.Session (withConnectionIO)
import Test.HUnit
import Control.Monad (liftM, when)
import qualified Jakway.Blackjack.IO.Database as DB
import Data.List (sort)

test_db_name = "tmp_test.db"

--see http://stackoverflow.com/questions/8502201/remove-file-if-it-exists
removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

withDatabase name = withConnectionIO (connectSqlite3 name)
withTestDatabase = withDatabase test_db_name


testOpenDatabase = TestCase $ withTestDatabase $ (\_ -> do
                                exists <- doesFileExist test_db_name
                                if exists
                                    then return ()
                                    else assertFailure message)
                    where message = "Database "++test_db_name++" does not exist!"

testTableList = TestCase $ withTestDatabase $ \conn -> DB.initializeDatabase conn >> commit conn >> getTables conn >>= (\tables -> when (not $ tablesEqual tables) (assertFailure $ message tables))
                where tables = ["cards", "players", "hands", "matches"]
                      tablesEqual readTables = (sort tables) == (sort readTables)
                      message readTables = "Database tables don't match!  Read tables: " ++ (show readTables)

tests = TestList [TestLabel "testOpenDatabase" testOpenDatabase, TestLabel "testTableList" testTableList]
