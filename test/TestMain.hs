module TestMain where

import qualified Jakway.Blackjack.Tests.DatabaseTests

main = runTestTT DatabaseTests.tests
