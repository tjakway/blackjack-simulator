module Jakway.Blackjack.Tests.DatabaseTests.MultiTableTests (tests) where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import qualified Jakway.Blackjack.IO.DatabaseCommon as DB
import qualified Jakway.Blackjack.IO.DatabaseWrites as DB
import qualified Jakway.Blackjack.IO.DatabaseReads as DB
import Jakway.Blackjack.Tests.DatabaseTests.Common

basicTwoTableNames = map DB.getTableNames ["t1", "t2"]
alphabeticTableNames = map DB.getTableNames ["abc", "def", "ghi", "jkl", "mno", "pqr", "stu", "wxy", "z"]
numericTableNames = map DB.getTableNames ["123", "456", "298512003", "3", "10", "34986092809809809322022"]

createMultiTableTests :: Assertion
createMultiTableTests = withTempDatabase $ \conn -> do
       map DB.createTables $ basicTwoTableNames ++ alphabeticTableNames ++ numericTableNames


tests = testGroup "MultiTableTests" [testCase "createMultiTableTests" createMultiTableTests]
