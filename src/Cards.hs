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
type Hand = [Visibility Card]

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
drawCard :: Deck -> (Card, Deck)
drawCard (x:xs) = (x, xs)

hasCard :: [Card] -> CardValue -> Bool
hasCard cards whichCard = (elem True) . fmap ((==whichCard) . cardValue) $ cards

blackjack :: [Card] -> Bool
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

handPoints :: [Card] -> Int
handPoints hand = let total = sum $ fmap (cardPoints . cardValue) hand
                     in if total <= 21 then total
                                       else total - 10

isBust :: [Card] -> Bool
isBust hand = let total = handPoints hand
                  in if total > 21 then True
                                   else False

data Record = Record { wins :: Integer,
                       ties :: Integer,
                       losses :: Integer }

data Result = Win | Tie | Lose

data Visibility a = Hidden a | Shown a

--any better way to do this?
unwrapVisibility :: Visibility a -> a
unwrapVisibility (Hidden a) = a
unwrapVisibility (Shown a) = a

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

class AI a where
        --returns the resulting deck and the players hand
        --only need to call this once because all moves are decided one
        --player at a time, i.e.
        --one player makes all his moves before the next player does
        play :: a -> Deck -> Hand -> (Deck, Hand)

data AIType = BasicDealer | BasicPlayer

instance AI AIType where
        play BasicDealer deck myHand = let cards = map unwrapVisibility myHand
                                           points = handPoints cards
                                               --any way to use a case statement or guards here?
                                          in if (points < 17) then hitMe
                                                              else (deck, myHand)
                                            --draw cards face down
                                        where (drawnCard, resultingDeck) = drawCard deck :: (Card, Deck)
                                              hitMe = play BasicDealer resultingDeck ((return drawnCard) : myHand)

        play BasicPlayer deck myHand = play BasicDealer deck myHand

playGame :: (AI a, AI b) => a -> [b] -> Deck -> Maybe (Result, [Result])
-- |Can't play a game without any players
playGame dealer [] deck = Nothing
playGame dealer allPlayers deck = undefined

-- |first result in the tuple = result for the first Hand
-- |second result in the tuple = result for the second Hand
-- this function is very repetitive--rewrite it to pass a tuple of Hands
-- instead of each players hand as a separate variable
-- so it'll be:
-- whoWon :: (Hand, Hand) -> (Result, Result)
whoWon :: Hand -> Hand -> (Result, Result)
whoWon firstPlayerHand secondPlayerHand 
                                        --if both the dealer and a player
                                        --bust, it's a tie
                                        | firstPlayerBusted && secondPlayerBusted = (Tie, Tie)
                                        --check if one player busted and
                                        --the other didn't
                                        | firstPlayerBusted && (not secondPlayerBusted) = (Lose, Win)
                                        | (not firstPlayerBusted) && secondPlayerBusted = (Win, Lose)
                                        --if neither player busted, highest
                                        --score wins
                                        | firstPlayerScore == secondPlayerScore = (Tie, Tie)
                                        | firstPlayerScore > secondPlayerScore = (Win, Lose)
                                        | firstPlayerScore < secondPlayerScore = (Lose, Win)

                                --any way to rewrite this in applicative
                                --syntax?
                                where playerBusted playerHand = (let cards = fmap unwrapVisibility firstPlayerHand in isBust cards) :: Bool
                                      firstPlayerBusted = playerBusted firstPlayerHand
                                      secondPlayerBusted = playerBusted secondPlayerHand
                                      firstPlayerScore = handPoints $ fmap unwrapVisibility firstPlayerHand
                                      secondPlayerScore = handPoints $ fmap unwrapVisibility secondPlayerHand



