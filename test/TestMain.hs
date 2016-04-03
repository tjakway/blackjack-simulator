module Main where

import qualified Jakway.Blackjack.Tests.DatabaseTests.BasicTests as BasicTests
import qualified Jakway.Blackjack.Tests.DatabaseTests.MultiTableTests as MultiTableTests
import qualified Jakway.Blackjack.Tests.AITests.BasicPlayerTests as BasicPlayerTests
import qualified Jakway.Blackjack.Tests.IntegrationTests.MatchTests as MatchTests
import qualified Jakway.Blackjack.Tests.IntegrationTests.MainTests as MainTests
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid

main :: IO ()
main = defaultMainWithOpts 
            [BasicTests.tests, MultiTableTests.tests, integrationTests, aiTests]
            mempty
        where integrationTests = testGroup "Integration Tests" (MatchTests.testCases ++ MainTests.testCases)
              aiTests = testGroup "AI Tests" BasicPlayerTests.testCases
