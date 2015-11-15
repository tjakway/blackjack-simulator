module Cards where

import Control.Applicative
import Data.Monoid
import Data.Ord
import Data.Foldable
import System.Random
import System.Random.Shuffle
import Control.Monad.State
import Control.Monad
import Data.List

data Suit
  = Spade 
  | Club 
  | Heart
  | Diamond
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

data CardValue
  = Two 
  | Three 
  | Four 
  | Five 
  | Six 
  | Seven 
  | Eight 
  | Nine 
  | Ten 
  | Jack  
  | Queen
  | King
  | Ace
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Card = Card
  { cardSuit :: Suit
  , cardValue :: CardValue
  } deriving Show

--disambiguate between a player's hand and the deck--both are lists of cards
type Deck = [Card]
type Hand = [Visibility Card]

data Result = Lose | Tie | Win deriving (Eq, Ord, Bounded)

data Visibility a = Hidden a | Shown a

-- | add the passed result to the total and return the new total
addResult :: ScoreRecord -> Result -> ScoreRecord
-- could have used guards here, but I wanted practice using case
addResult (ScoreRecord prevWins prevTies prevLosses) res = 
  case res of
       Win -> ScoreRecord (prevWins+1) prevTies prevLosses
       Tie -> ScoreRecord prevWins (prevTies+1) prevLosses
       Lose -> ScoreRecord prevWins prevTies (prevLosses+1)

instance Monoid ScoreRecord where
  mempty = ScoreRecord 0 0 0
  --XXX
  --there has got to be a better way of doing this
  --maybe redefine ScoreRecord as a tuple (Integer, Integer, Integer)?
  mappend (ScoreRecord firstWins firstTies firstLosses) (ScoreRecord secondWins secondTies secondLosses) = ScoreRecord (firstWins + secondWins) (firstTies + secondTies) (firstLosses + secondLosses)

unwrapVisibility :: Visibility a -> a
unwrapVisibility (Hidden a) = a
unwrapVisibility (Shown a) = a

instance Functor Visibility where
  fmap f (Hidden a) = Hidden (f a)
  fmap f (Shown a) = Shown (f a)

instance Monad Visibility where
  --cards are shown by default
  return = Shown

  (>>=) (Shown a) f  = f a
  (>>=) (Hidden a) f = f a

instance Applicative Visibility where
  pure = return
  (Hidden f) <*> b = fmap f b
  (Shown f) <*> b = fmap f b

data ScoreRecord
  = ScoreRecord 
  { wins :: Integer
  , ties :: Integer
  , losses :: Integer
  }

type Blackjack a = State Deck a

class AI a where
  play :: a -> Hand -> Deck -> (Hand, Deck)

data BasicDealer = BasicDealer
data BasicPlayer = BasicPlayer

instance AI BasicDealer where
  play BasicDealer myHand deck = flip runState deck $ do
      let points = handPoints (map unwrapVisibility myHand)
      if points < 17
         then do
           drawnCard <- drawCard
           deck' <- get
           return . fst $ play BasicDealer (Shown drawnCard : myHand) deck'
         else 
           return myHand


instance AI BasicPlayer where
  play BasicPlayer = play BasicDealer

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
infiniteShuffledDeck gen = shuffledDeck ++ infiniteShuffledDeck gen
  where shuffledDeck = shuffleDeck gen newDeck


-- |draws 1 card and returns a tuple of that card and the resulting deck
-- this function intentionally DOES NOT pattern match on []--the deck is
-- supposed to be infinite so if we got an empty list it's a bug
drawCard :: Blackjack Card
drawCard = do
  card <- gets head
  modify tail
  return card


drawCard' :: Deck -> (Card, Deck)
drawCard' (x:xs) = (x, xs)

hasCard :: [Card] -> CardValue -> Bool
hasCard cards whichCard = any ((whichCard ==) . cardValue) cards

blackjack :: [Card] -> Bool
blackjack hand = 2 == length hand && hasAce && hasFaceCard
  where
    handHas = hasCard hand
    hasAce = handHas Ace
    faceCards = Card <$> allSuits <*> [Jack, Queen, King]
    hasFaceCard = any (handHas . cardValue) faceCards
    

cardPoints :: CardValue -> Int
cardPoints cardValue 
  | cardValue == Jack  = 10
  | cardValue == Queen = 10
  | cardValue == King  = 10
  --count aces as 11 now, can decrement 10 later as necessary
  | cardValue == Ace   = 11
  --enums count up from 0 but the first card type is 2
  | otherwise = 2 + fromEnum cardValue

handPoints :: [Card] -> Int
handPoints hand = 
  if total <= 21
     then total
     else total - 10
  where
    total = getSum $ foldMap (Sum . cardPoints . cardValue) hand

isBust :: [Card] -> Bool
isBust hand = 21 < handPoints hand

startingHand :: Blackjack Hand
startingHand = do
  firstCard <- Hidden <$> drawCard
  secondCard <- Shown <$> drawCard
  return [firstCard, secondCard]


playGame :: (AI a, AI b) => a -> [b] -> Deck -> Maybe (ScoreRecord, [Result])
-- |Can't play a game without any players
playGame dealerAI [] deck = Nothing
playGame dealerAI allPlayers deck = flip evalState deck $ do
  dealersStartingHand <- startingHand
  playerHands <- reverse <$> mapM foldFnSt allPlayers
  dealerHand <- play' dealerAI dealersStartingHand
  let results = reverse . map (whoWon' dealerHand)
  return . Just . first dealerScore . join (,) . results $ playerHands
  where
    dealerScore = foldr (flip addResult) mempty
    foldFnSt ai = startingHand >>= play' ai

infixl 8 &&&
(f &&& g) a = (f a, g a)



first :: (a -> b) -> (a, c) -> (b, c)
first f (a, b) = (f a, b)

both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b)

play' :: (AI a) => a -> Hand -> Blackjack Hand
play' ai hand = do
  deck <- get
  let (resultingHand, resDeck) = play ai hand deck
  put resDeck
  return resultingHand


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
  | firstPlayerBusted && not secondPlayerBusted = (Lose, Win)
  | not firstPlayerBusted && secondPlayerBusted = (Win, Lose)
  --if neither player busted, highest
  --score wins
  | firstPlayerScore == secondPlayerScore = (Tie, Tie)
  | firstPlayerScore > secondPlayerScore = (Win, Lose)
  | firstPlayerScore < secondPlayerScore = (Lose, Win)
  
  --any way to rewrite this in applicative
  --syntax?

  -- Probably not, but... notice that you only need one part of the info here.
  -- If the function just returns whether or not the first person won,
  -- then we can cut out half of the cases.
  where
    -- You were using a wrong variable here. `playerBusted`
    -- only referred to firstPlayerHand. One area where point-free
    -- is nice is not using the wrong variables!
    playerBusted = isBust . map unwrapVisibility 
    firstPlayerBusted = playerBusted firstPlayerHand
    secondPlayerBusted = playerBusted secondPlayerHand
    firstPlayerScore = handPoints $ map unwrapVisibility firstPlayerHand
    secondPlayerScore = handPoints $ map unwrapVisibility secondPlayerHand
  
whoWon' :: Hand -> Hand -> Result
whoWon' first second = ordToResult $ bust <> scores
  where
    busted = isBust . map unwrapVisibility 
    bust = comparing busted first second
    scores = comparing (handPoints . map unwrapVisibility) first second

ordToResult :: Ordering -> Result
ordToResult GT = Win
ordToResult LT = Lose
ordToResult EQ = Tie
