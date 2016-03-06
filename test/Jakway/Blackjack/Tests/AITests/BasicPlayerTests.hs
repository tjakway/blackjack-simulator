module Jakway.Blackjack.Tests.AITests.BasicPlayerTests (testCases) where

import Jakway.Blackjack.Tests.Constants
import Jakway.Blackjack.CardOps
import Jakway.Blackjack.AI
import Jakway.Blackjack.Match
import Jakway.Blackjack.Game
import Data.Maybe (fromJust)
import Control.Monad.State (evalState)
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

sanityCheck :: Assertion
sanityCheck = let resCards = (flip evalState) testDeck $ do
                                    fstCard <- drawCard
                                    sndCard <- drawCard
                                    return [fstCard, sndCard]
                  message = "Should have drawn 2 cards."
                  in assertBool message (length resCards == 2)

tie_1v1 :: Assertion
tie_1v1 = do
        let (Match dealerHand _ playerHand res) = test_1v1_game
        let message = "Both players should have 3 cards"
        assertBool message (length dealerHand == 3)
        assertBool message (length (head playerHand) == 3)

testCases = [testCase "sanityCheck" sanityCheck, testCase "tie_1v1" tie_1v1]
