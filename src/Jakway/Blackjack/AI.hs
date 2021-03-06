module Jakway.Blackjack.AI where

import Jakway.Blackjack.Visibility
import Jakway.Blackjack.CardOps
import Jakway.Blackjack.Cards
import Jakway.Blackjack.Points
import Control.Monad.State
import System.Random

data AI = BasicDealer 
          | FiftyFiftyDealer
          | BasicPlayer 
          | FiftyFiftyPlayer
        deriving (Show, Read, Eq)

type AIProc = [Hand] -> Blackjack Hand

-- |which AI we are, other players' hands (index 0 is the dealer), our hand
-- and the deck
play :: AI -> Hand -> [Hand] -> Deck -> (Hand, Deck)
play BasicDealer myHand _ deck = flip runState deck $ do
    let points = handPoints (map unwrapVisibility myHand)
    if points < 17
        then do
            drawnCard <- drawCard
            state $ play BasicDealer (Shown drawnCard : myHand) []
        else 
        return myHand

-- |currently all players play the same
play BasicPlayer myHand _ deck = play BasicDealer myHand [] deck
play FiftyFiftyPlayer myHand _ deck = if isBust $ map unwrapVisibility myHand 
                                        then stand myHand deck
                                        else fiftyfifty deck (hit FiftyFiftyPlayer myHand [] deck) (stand myHand deck)
        where points = handPoints (map unwrapVisibility myHand)
              --uses the deck as a source of randomness
              --has a 50% chance of calling f, 50% chance of calling 
              fiftyfifty deck f g = let randFlag = fst . random . deckToRNG $ deck
                                            in if randFlag == True then f
                                                                   else g

play FiftyFiftyDealer h ph d = play FiftyFiftyPlayer h ph d

hit :: AI -> Hand -> [Hand] -> Deck -> (Hand, Deck)
hit ai hand otherHands deck = flip runState deck $ do
    drawnCard <- drawCard
    state $ play ai (Shown drawnCard : hand) otherHands

stand :: Hand -> Deck -> (Hand, Deck)
stand = (,)

-- |TODO: write a test comparing the randomness of deckToRNG to the
-- built-in StdGen
-- see e.g. http://www.cse.wustl.edu/~jain/cse567-08/ftp/k_27trg.pdf
deckToRNG :: Deck -> StdGen
--draw a card from the deck at an arbitrary position and use it to seed a RNG
--the randomness comes from the fact that the deck is shuffled
deckToRNG deck = let (Card suit cardVal) = deck !! position
                     --add 1 in case either value is 0
                     in mkStdGen $ ((fromEnum suit) + 1) * ((fromEnum cardVal) + 1)
        where position = 13
