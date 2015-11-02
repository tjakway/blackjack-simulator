module Cards
(
Suit, CardValue, Card, newDeck, shuffleDeck
) where

import Control.Applicative
import System.Random
import System.Random.Shuffle
import Control.Monad.State

data Suit = Spade | Club | Heart | Diamond
               deriving (Eq, Ord, Enum, Bounded, Show, Read)
data CardValue = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
               deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Card = Card
  { cardSuit :: Suit
  , cardValue :: CardValue
  } deriving (Show)

newDeck :: [Card]
newDeck = Card <$> allSuits <*> allCardValues
        where allSuits = [minBound..maxBound] :: [Suit]
              allCardValues = [minBound..maxBound] :: [CardValue]

shuffleDeck :: (RandomGen a) => (a, [Card]) -> [Card]
shuffleDeck (rand, cards) = let listLength = fst . next $ rand
                                randList = [1..listLength]
                                in shuffle cards randList
