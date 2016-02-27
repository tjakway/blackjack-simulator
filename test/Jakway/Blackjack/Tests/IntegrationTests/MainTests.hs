module Jakway.Blackjack.Tests.IntegrationTests.MainTests (testCases) where

import Test.HUnit hiding (State)
import Test.Framework
import Test.Framework.Providers.HUnit
import System.Environment (withArgs)
import Jakway.Blackjack.IO.TableNames
import Database.HDBC


testInsertOneMatch :: Assertion
testInsertOneMatch = undefined
--        withArgs []


assertTablesExist :: (IConnection a) => a -> TableNames -> Assertion
assertTablesExist conn n = getTables conn >>= containsNames
            where containsNames tables = 
                        let ctn = "cards" --also make sure the cards table exists
                            ptn = getPlayerTableName n
                            htn = getHandTableName n
                            mtn = getMatchTableName n
                            message = "Table not found!"
                            in assertBool message (ctn `elem` tables && ptn `elem` tables && htn `elem` tables && mtn `elem` tables)


testCases = [testCase "testInsertOneMatch" testInsertOneMatch]
