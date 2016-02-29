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

sandboxTables :: IO () -> IO ()
sandboxTables action = connectDB >>= ((flip withTransaction) $ \f_conn -> dropAllTables f_conn >> commit f_conn >> disconnect f_conn >> action) >> connectDB >>= (dropAllTables >> commit)

basicArgs :: String -> [String]
basicArgs suffix = ["-v", "--with-dealer=BasicDealer", "--num-BasicPlayer=1", "--num-games=10", "--tablename-suffix=" ++ suffix]

-- |Make sure that the program correctly sets up tables"
testRunTables :: Assertion
testRunTables = do
        pre_run_conn <- connectDB
        dropAllTables pre_run_conn
        commit pre_run_conn
        disconnect pre_run_conn

        let suffix = "test_ins_one_match_suff"
        Env.withArgs (basicArgs suffix) ProgMain.progMain

        conn <- connectDB
        assertTablesExist conn (getTableNames suffix)
        dropAllTables conn
        commit conn

        tables <- getTables conn
        assertEqual "dropAllTables failed!" tables []

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
