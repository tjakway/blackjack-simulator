module Jakway.Blackjack.Tests.AITests.BasicPlayerTests (testCases) where

import Jakway.Blackjack.Tests.Constants
import Jakway.Blackjack.CardOps
import Jakway.Blackjack.Cards
import Jakway.Blackjack.Visibility
import Jakway.Blackjack.AI
import Jakway.Blackjack.Match
import Jakway.Blackjack.Game
import Data.Maybe (fromJust)
import Control.Monad.State (evalState)
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Jakway.Blackjack.Result
import Data.List (sort)

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


testPlayerBust :: Assertion
testPlayerBust = let maybeMatch = evalGame BasicDealer [BasicPlayer] deck
                     --dealer has 21
                     expectedDealerHand = [Hidden $ Card {cardSuit = Club, cardValue = Four},Shown $ Card {cardSuit = Heart, cardValue = Three},Shown $ Card {cardSuit = Heart, cardValue = Six},Shown $ Card {cardSuit = Diamond, cardValue = Seven}]
                     
                     --player busts
                     expectedPlayerHand = [Hidden $ Card {cardSuit = Spade, cardValue = Nine},Shown $ Card {cardSuit = Heart, cardValue = Eight},Shown $ Card {cardSuit = Club, cardValue = Six}]
                     expectedResult = Lose
                    in case maybeMatch of Nothing -> assertFailure "Failed to run match"
                                          Just (Match dHand pIds pHands pRes) -> assertEqual "Dealer's hand didn't match" (sort expectedDealerHand) (sort dHand) >> 
                                                                assertEqual "Player's hand didn't match" (sort expectedPlayerHand) (sort . head $ pHands) >> assertEqual "Incorrect result" expectedResult (head pRes)
        where deck = [Card {cardSuit = Spade, cardValue = Nine},Card {cardSuit = Heart, cardValue = Eight},Card {cardSuit = Club, cardValue = Six},
                     Card {cardSuit = Club, cardValue = Four},Card {cardSuit = Heart, cardValue = Three},Card {cardSuit = Heart, cardValue = Six},Card {cardSuit = Diamond, cardValue = Seven},
                     Card {cardSuit = Spade, cardValue = Ace},Card {cardSuit = Heart, cardValue = Ten},Card {cardSuit = Club, cardValue = Ace}]

testBlackjackTie :: Assertion
testBlackjackTie =  case maybeMatch of Nothing -> assertFailure "Match failed"
                                       Just (Match dHand pIds [pHand] [pRes]) -> assertEqual "Dealers hand didn't match" (sort expectedDealerHand) (sort dHand) >> assertEqual "Player's hand didn't match" (sort expectedPlayerHand) (sort pHand) >> assertEqual "Outcome didn't match" Tie pRes
    where expectedDealerHand = [Hidden $ Card Spade Ace, Shown $ Card Spade King]
          expectedPlayerHand = [Hidden $ Card Club King, Shown $ Card Heart Ace]
          deck = map unwrapVisibility $ expectedDealerHand ++ expectedPlayerHand
          maybeMatch = evalGame BasicDealer [BasicPlayer] deck

testCases = [testCase "sanityCheck" sanityCheck, testCase "tie_1v1" tie_1v1, testCase "testPlayerBust" testPlayerBust, testCase "testBlackjackTie" testBlackjackTie]
