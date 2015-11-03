module Cards where

import Control.Applicative
import System.Random
import System.Random.Shuffle
import Control.Monad.State
import Control.Monad
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

isFaceCard :: Card -> Bool
isFaceCard (Card _ Jack)  = True
isFaceCard (Card _ Queen) = True
isFaceCard (Card _ King)  = True
isFaceCard (Card _ _)     = False

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

cardPoints :: CardValue -> Int
cardPoints cardValue 
                    | cardValue == Jack  = 10
                    | cardValue == Queen = 10
                    | cardValue == King  = 10
                    --count aces as 11 now, can decrement 10 later as necessary
                    | cardValue == Ace   = 11
                    --enums count up from 0 but the first card type is 2
                    | otherwise = (+2) . fromEnum $ cardValue

handPoints :: Hand -> Int
handPoints hand = let total = sum $ fmap (cardPoints . cardValue) hand
                     in if total <= 21 then total
                                       else total - 10

isBust :: Hand -> Bool
isBust hand = let total = handPoints hand
                  in if total > 21 then True
                                   else False

data Record = Record { wins :: Integer,
                       ties :: Integer,
                       losses :: Integer }

data Result = Win | Tie | Lose

data Visibility a = Hidden a | Shown a

instance Functor Visibility where
        fmap f (Hidden a) = Hidden (f a)
        fmap f (Shown a) = Shown (f a)

instance Monad Visibility where
        --cards are shown by default
        return a = Shown a

        (>>=) (Shown a) f  = f a
        (>>=) (Hidden a) f = f a

instance Applicative Visibility where
        pure = return
        (Hidden f) <*> b = fmap f b
        (Shown f) <*> b = fmap f b
