{-# LANGUAGE CPP #-}
module Jakway.Blackjack.Tests.IntegrationTests.MainTests (testCases) where

import qualified Jakway.Blackjack.ProgMain as ProgMain
import Test.HUnit hiding (State)
import Test.Framework
import Test.Framework.Providers.HUnit
import qualified System.Environment as Env
import Jakway.Blackjack.IO.TableNames
import Jakway.Blackjack.IO.DatabaseConnection
import Jakway.Blackjack.IO.DatabaseCommon
import Jakway.Blackjack.IO.DatabaseWrites
import Jakway.Blackjack.IO.DatabaseReads
import Database.HDBC
import System.Random
import Control.Monad

connectDB :: IConnection a => IO a
-- TODO: implement for SQLite
#ifdef BUILD_POSTGRESQL
connectDB = connectPostgresDBReadString
#else

#endif

--Test utilities:
-- ********************************************************

-- |delete all tables, run action, then delete all tables again
-- lets you run tests with a clean slate
sandboxTables :: IO () -> IO ()
sandboxTables action = connectDB >>= (\f_conn -> dropAllTables f_conn >> commit f_conn >> action >> dropAllTables f_conn >> commit f_conn)

mainTestsSuffix = "msuff"
mainTestsTableNames = getTableNames mainTestsSuffix

basicArgs :: String -> [String]
basicArgs suffix = ["-v", "--with-dealer=BasicDealer", "--num-BasicPlayer=1", "--num-games=10", "--tablename-suffix=" ++ suffix]

-- |parameter: TableName suffix
mainWithBasicArgs :: IO ()
mainWithBasicArgs = Env.withArgs (basicArgs mainTestsSuffix) ProgMain.progMain
-- ********************************************************

-- |Make sure that the program correctly sets up tables"
testRunTables :: Assertion
testRunTables =  (sandboxTables $ mainWithBasicArgs) >> connectDB >>= getTables >>= (assertEqual "dropAllTables failed!" [])
            where suffix = "test_ins_one_match_suff"
        

testInsertTenMatches :: Assertion
testInsertTenMatches = sandboxTables $ do
            mainWithBasicArgs
            conn <- connectDB
            numPlayers <- getNumPlayers conn mainTestsTableNames
            let expectedNumPlayers = 2
            assertEqual ("Incorrect number of players!  Expected: " ++ (show expectedNumPlayers)) expectedNumPlayers numPlayers
            numMatches <- getNumMatches conn mainTestsTableNames 
            let expectedNumMatches = 10
            assertEqual ("Incorrect number of matches!  Expected: " ++ (show expectedNumMatches)) expectedNumMatches numMatches

testReadWriteXMatches :: Int -> Integer -> Assertion
testReadWriteXMatches expectedNumPlayers expectedMatches = sandboxTables $ do
            Env.withArgs args ProgMain.progMain
            conn <- connectDB
            numPlayers <- getNumPlayers conn mainTestsTableNames
            assertEqual ("Incorrect number of players!  Expected: " ++ (show expectedNumPlayers)) expectedNumPlayers numPlayers
            numMatches <- getNumMatches conn mainTestsTableNames 
            assertEqual ("Incorrect number of matches!  Expected: " ++ (show expectedMatches)) expectedMatches numMatches
    where numExpectedPlayers = 2
          args = ["-v", "--with-dealer=BasicDealer", "--num-BasicPlayer=" ++ (show expectedNumPlayers), "--num-games=" ++ (show expectedMatches), "--tablename-suffix=" ++ mainTestsSuffix]

testReadWriteRandomMatches :: Assertion
testReadWriteRandomMatches = getStdGen >>= (\gen -> (gen,randMatches gen) >>= 
       ( \(prev_gen, rM) -> (rM, randPlayers . split $ gen) >>= uncurry testReadWriteXMatches))
        where randInRange min max gen = tuplify2 . (take 2) . (liftM $ randomRs (minMatches, maxMatches)) $ gen
              randMatches = randInRange minMatches maxMatches
              randPlayers = randInRange minPlayers maxPlayers
              minMatches = 2
              maxMatches = 100
              minPlayers = 2
              maxPlayers = 5
              --see http://stackoverflow.com/questions/2921345/how-do-i-convert-a-list-to-a-tuple-in-haskell
              tuplify2 [x,y] = (x,y)

assertTablesExist :: (IConnection a) => a -> TableNames -> Assertion
assertTablesExist conn n = getTables conn >>= containsNames
            where containsNames tables = 
                        let ctn = "cards" --also make sure the cards table exists
                            ptn = getPlayerTableName n
                            htn = getHandTableName n
                            mtn = getMatchTableName n
                            message = "Table not found!"
                            in assertBool message (ctn `elem` tables && ptn `elem` tables && htn `elem` tables && mtn `elem` tables)


testCases = [testCase "testRunTables" testRunTables, testCase "testInsertTenMatches" testInsertTenMatches]
