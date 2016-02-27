module Main where

import qualified Jakway.Blackjack.Tests.DatabaseTests.BasicTests as BasicTests
import qualified Jakway.Blackjack.Tests.DatabaseTests.MultiTableTests as MultiTableTests
import qualified Jakway.Blackjack.Tests.GameTests as GameTests
import qualified Jakway.Blackjack.Tests.IntegrationTests.MatchTests as MatchTests
import qualified Jakway.Blackjack.Tests.IntegrationTests.MainTests as MainTests
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

main :: IO ()
main = defaultMainWithOpts 
            [BasicTests.tests, MultiTableTests.tests, GameTests.tests, integrationTests]
            mempty
        where integrationTests = testGroup "IntegrationTests" (MatchTests.testCases ++ MainTests.testCases)
