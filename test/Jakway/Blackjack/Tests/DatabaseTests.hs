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
import Control.Monad (liftM, unless)
-- |overlapping for convenience--change if necessary
import qualified Jakway.Blackjack.IO.DatabaseWrites as DB
import qualified Jakway.Blackjack.IO.DatabaseReads as DB
import Jakway.Blackjack.Cards
import Jakway.Blackjack.Visibility
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

-- |run the 
withDatabase name = withConnectionIO' (connectSqlite3 name)

-- |run a transaction on a database that will be deleted before and after running it
withTempDatabase transaction dbName = removeIfExists dbName >> withDatabase dbName transaction >> removeIfExists dbName

-- |initialize the database then run the transaction
-- don't forget to commit!
withTestDatabase transaction = withTempDatabase (\conn -> DB.enableForeignKeys conn >> DB.initializeDatabase conn >> commit conn >> transaction conn) test_db_name


testOpenDatabase :: Assertion
testOpenDatabase = withTestDatabase $ (\_ -> do
                                exists <- doesFileExist test_db_name
                                if exists
                                    then return ()
                                    else assertFailure message)
                    where message = "Database "++test_db_name++" does not exist!"

testTableList :: Assertion
testTableList =  withTestDatabase $ \conn -> getTables conn >>= (\tables -> unless (tablesEqual tables) (assertFailure $ message tables))
                where tables = ["cards", "players", "hands", "matches"]
                      -- | in case sqlite adds an extra schema table
                      tablesEqual readTables = (sort tables) == (sort . (delete "sqlite_sequence") $ readTables)
                      message readTables = "Database tables don't match!  Read tables: " ++ (show readTables)


getNumPlayers :: (IConnection a) => a -> IO (Int)
getNumPlayers conn = do
        query <- prepare conn "SELECT * FROM players"
        execute query []
        rows <- fetchAllRows' query
        return . length $ rows

testInsertPlayers :: Assertion
testInsertPlayers = withTestDatabase $ \conn -> do
                        let numPlayers = 10
                        DB.insertPlayers conn numPlayers
                        commit conn
                        numPlayerRows <- getNumPlayers conn
                        let message = "numPlayerRows is "++(show numPlayerRows)++" (should be"++(show numPlayers)++")"
                        assertBool message (numPlayers == numPlayerRows) 

testInsertOneHand :: Assertion
testInsertOneHand = withTestDatabase $ \conn -> do
       let whichPlayer = 1
       DB.insertPlayers conn 2
       let hand = [Hidden (Card Spade Ace), Shown (Card Diamond King)] 
       insertStatement <- DB.insertHandStatement conn
       whichHand <- DB.insertHand insertStatement conn whichPlayer hand
       commit conn

       readStatement <- DB.readHandStatement conn 
       res <- DB.readHand readStatement whichHand
       case res of Nothing -> assertFailure "Could not read hand id!"
                   Just resHand -> assertEqual "" hand resHand


tests =  [testCase "testOpenDatabase" testOpenDatabase, testCase "testTableList" testTableList, testCase "testInsertPlayers" testInsertPlayers]
