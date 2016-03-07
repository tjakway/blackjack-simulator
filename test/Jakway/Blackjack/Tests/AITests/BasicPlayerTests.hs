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


testDealerWin :: Assertion
testDealerWin = let match = evalGame BasicDealer [BasicPlayer] deck

        where deck = [Card {cardSuit = Club, cardValue = Four},Card {cardSuit = Heart, cardValue = Three},Card {cardSuit = Heart, cardValue = Six},Card {cardSuit = Diamond, cardValue = Seven},Card {cardSuit = Spade, cardValue = Nine},Card {cardSuit = Heart, cardValue = Eight},Card {cardSuit = Club, cardValue = Six},Card {cardSuit = Spade, cardValue = Ace},Card {cardSuit = Heart, cardValue = Ten},Card {cardSuit = Club, cardValue = Ace},Card {cardSuit = Spade, cardValue = Four},Card {cardSuit = Club, cardValue = Three},Card {cardSuit = Diamond, cardValue = Eight},Card {cardSuit = Diamond, cardValue = Jack},Card {cardSuit = Spade, cardValue = Seven},Card {cardSuit = Diamond, cardValue = Three},Card {cardSuit = Diamond, cardValue = Ten},Card {cardSuit = Heart, cardValue = Two},Card {cardSuit = Heart, cardValue = Queen},Card {cardSuit = Spade, cardValue = Five},Card {cardSuit = Heart, cardValue = King},Card {cardSuit = Heart, cardValue = Five},Card {cardSuit = Club, cardValue = Eight},Card {cardSuit = Spade, cardValue = Eight},Card {cardSuit = Spade, cardValue = Six},Card {cardSuit = Diamond, cardValue = Six},Card {cardSuit = Diamond, cardValue = Queen},Card {cardSuit = Spade, cardValue = King},Card {cardSuit = Diamond, cardValue = Nine},Card {cardSuit = Club, cardValue = Nine},Card {cardSuit = Heart, cardValue = Nine},Card {cardSuit = Spade, cardValue = Two},Card {cardSuit = Diamond, cardValue = Two},Card {cardSuit = Diamond, cardValue = Four},Card {cardSuit = Spade, cardValue = Jack},Card {cardSuit = Club, cardValue = Five},Card {cardSuit = Heart, cardValue = Four},Card {cardSuit = Diamond, cardValue = King},Card {cardSuit = Club, cardValue = Ten},Card {cardSuit = Heart, cardValue = Jack},Card {cardSuit = Diamond, cardValue = Ace},Card {cardSuit = Club, cardValue = King},Card {cardSuit = Club, cardValue = Two},Card {cardSuit = Diamond, cardValue = Five},Card {cardSuit = Heart, cardValue = Seven},Card {cardSuit = Club, cardValue = Jack},Card {cardSuit = Spade, cardValue = Three},Card {cardSuit = Spade, cardValue = Queen},Card {cardSuit = Club, cardValue = Queen},Card {cardSuit = Spade, cardValue = Ten},Card {cardSuit = Club, cardValue = Seven},Card {cardSuit = Heart, cardValue = Ace}]


testCases = [testCase "sanityCheck" sanityCheck, testCase "tie_1v1" tie_1v1]
