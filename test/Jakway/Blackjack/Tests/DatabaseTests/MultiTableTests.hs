module Jakway.Blackjack.Tests.DatabaseTests.MultiTableTests (tests) where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import qualified Jakway.Blackjack.IO.DatabaseCommon as DB
import qualified Jakway.Blackjack.IO.DatabaseWrites as DB
import qualified Jakway.Blackjack.IO.DatabaseReads as DB
import qualified Jakway.Blackjack.IO.TableNames as DB
import Jakway.Blackjack.Tests.DatabaseTests.Common

basicTwoTableNames = map DB.getTableNames ["t1", "t2"]
alphabeticTableNames = map DB.getTableNames ["abc", "def", "ghi", "jkl", "mno", "pqr", "stu", "wxy", "z"]
numericTableNames = map DB.getTableNames ["123", "456", "298512003", "3", "10", "34986092809809809322022"]

withMultiTableTestDatabase = withTempDatabase "multi_table_test.db"

createMultiTableTests :: Assertion
createMultiTableTests = withMultiTableTestDatabase $ \conn -> 
       sequence_ . (map $ DB.createTables conn) $ basicTwoTableNames ++ alphabeticTableNames ++ numericTableNames


tests = testGroup "MultiTableTests" [testCase "createMultiTableTests" createMultiTableTests]
