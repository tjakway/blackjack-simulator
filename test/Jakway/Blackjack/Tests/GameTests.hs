module Jakway.Blackjack.Tests.GameTests (tests) where

import Jakway.Blackjack.Tests.Constants
import Jakway.Blackjack.CardOps
import Control.Monad.State
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

tests = testGroup "GameTests" [testCase "sanityCheck" sanityCheck]
