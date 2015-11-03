module Cards where

import Control.Applicative
import System.Random
import System.Random.Shuffle
import Control.Monad.State
import Data.List

data Suit = Spade | Club | Heart | Diamond
               deriving (Eq, Ord, Enum, Bounded, Show, Read)
data CardValue = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
               deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Card = Card
  { cardSuit :: Suit
  , cardValue :: CardValue
  } deriving (Show)

--disambiguate between a player's hand and the deck--both are lists of cards
type Deck = [Card]
type Hand = [Card]

allSuits :: [Suit]
allSuits = [minBound..maxBound] :: [Suit]

allCardValues :: [CardValue]
allCardValues = [minBound..maxBound] :: [CardValue]

newDeck :: Deck
newDeck = Card <$> allSuits <*> allCardValues

-- |(strictly) shuffles an entire deck of cards
shuffleDeck :: (RandomGen a) => a -> Deck -> Deck
shuffleDeck gen cards = shuffle' cards (length cards) gen

infiniteShuffledDeck :: (RandomGen a) => a -> Deck
infiniteShuffledDeck gen = shuffledDeck ++ (infiniteShuffledDeck gen)
                                    where shuffledDeck = shuffleDeck gen newDeck

-- |draws 1 card and returns a tuple of that card and the resulting deck
-- this function intentionally DOES NOT pattern match on []--the deck is
-- supposed to be infinite so if we got an empty list it's a bug
drawCard :: Deck -> (Card, [Card])
drawCard (x:xs) = (x, xs)

hasCard :: [Card] -> CardValue -> Bool
hasCard cards whichCard = (elem True) . fmap ((==whichCard) . cardValue) $ cards

blackjack :: Hand -> Bool
blackjack hand = let hasAce = hasCard hand Ace 
                     faceCards = Card <$> allSuits <*> [Jack, Queen, King]
                     hasFaceCard = (elem True) . fmap (hasCard hand) . fmap (cardValue) $ faceCards
                     in ((==2) . length $ hand) && (hasAce && hasFaceCard)

handValue :: Hand -> Int
handValue  = undefined
