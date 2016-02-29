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
import Database.HDBC

-- TODO: implement for SQLite
#ifdef BUILD_POSTGRESQL
connectDB = connectPostgresDBReadString
#else

#endif

-- |delete all deletes, run action, then delete all tables again
sandboxTables :: IO () -> IO ()
sandboxTables action = connectDB >>= (\f_conn -> dropAllTables f_conn >> commit f_conn >> action >> dropAllTables f_conn >> commit f_conn)

basicArgs :: String -> [String]
basicArgs suffix = ["-v", "--with-dealer=BasicDealer", "--num-BasicPlayer=1", "--num-games=10", "--tablename-suffix=" ++ suffix]

-- |Make sure that the program correctly sets up tables"
testRunTables :: Assertion
testRunTables =  (sandboxTables $ Env.withArgs (basicArgs suffix) ProgMain.progMain) >> connectDB >>= getTables >>= (assertEqual "dropAllTables failed!" [])
            where suffix = "test_ins_one_match_suff"
        

testInsertTenMatches :: Assertion
testInsertTenMatches = undefined


assertTablesExist :: (IConnection a) => a -> TableNames -> Assertion
assertTablesExist conn n = getTables conn >>= containsNames
            where containsNames tables = 
                        let ctn = "cards" --also make sure the cards table exists
                            ptn = getPlayerTableName n
                            htn = getHandTableName n
                            mtn = getMatchTableName n
                            message = "Table not found!"
                            in assertBool message (ctn `elem` tables && ptn `elem` tables && htn `elem` tables && mtn `elem` tables)


testCases = [testCase "testRunTables" testRunTables]
