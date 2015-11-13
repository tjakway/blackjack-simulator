module Cards where

import Control.Applicative
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

data Result = Win | Tie | Lose

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
  -- foldr mappend mempty is the default definition, so we can delete it.

--any better way to do this?
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
  play :: a -> Hand -> Blackjack Hand

data BasicDealer = BasicDealer
data BasicDealer = BasicPlayer

instance AI BasicDealer where
  play BasicDealer myHand = do
      let points = handsPoints (map unwrapVisibility myHand)
      if points < 17
         then do
           drawnCard <- drawCard
           play BasicDealer (Shown drawnCard : hand)
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

hasCard :: [Card] -> CardValue -> Bool
hasCard cards whichCard = any ((==whichCard) . cardValue) cards

blackjack :: [Card] -> Bool
blackjack hand = let hasAce = hasCard hand Ace 
                     faceCards = Card <$> allSuits <*> [Jack, Queen, King]
                     --XXX: I really don't think composing fmap is the
                     --right way to do this...
                     hasFaceCard = any (hasCard hand) . fmap cardValue $ faceCards
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
isBust hand = 21 < handPoints hand

startingHand :: Deck -> (Hand, Deck)
startingHand deck = let run = (do
                              firstDeck <- get
                              let (firstCard, secondDeck) = drawCard firstDeck
                                  (secondCard, thirdDeck) = drawCard secondDeck
                              put thirdDeck 
                              return [Hidden firstCard, Shown secondCard]) :: State Deck Hand
                     in runState run deck


playGame :: (AI a, AI b) => a -> [b] -> Deck -> Maybe (ScoreRecord, [Result])
-- |Can't play a game without any players
playGame dealerAI [] deck = Nothing
playGame dealerAI allPlayers deck = let { (dealersStartingHand, deckAfterDealerDraws) = let { (dFirstCard, dFirstDeck)   = drawCard deck;
                                        (dSecondCard, dSecondDeck) = drawCard dFirstDeck ;
                                        }
                                        in  ([Hidden dFirstCard, Shown dSecondCard], dSecondDeck);
                                        --runState $ (drawCard >>= (\firstCard -> drawCard >>= (\secondCard -> return [Hidden firstCard, Shown secondCard]))) deck 
                                        (playerResDeck, playerHands) = 
                                          -- ^ (the deck after every player has made his move, a list of the player results in the order each player took his turn)
                                          -- XXX: refactor this monstrosity of nested let bindings
                                          let { foldRes = foldr (\thisAI (thisDeck, handsList) -> let { (thisPlayersStartingHand, deckAfterDraw) = startingHand deckAfterDealerDraws;
                                              (resDeck, resultingHand) = play thisAI deckAfterDraw thisPlayersStartingHand; }
                                           in (resDeck, resultingHand : handsList) ) (deckAfterDealerDraws, [[]]) allPlayers;
                                           } in (fst foldRes, reverse $ snd foldRes);
                                           -- ^ need to reverse the list of player hands because we're appending each player's hand to the front of the list but iterating head -> tail
                                           (dealerResDeck, dealerHand) = play dealerAI playerResDeck dealersStartingHand;
                                           -- | in blackjack each player faces off against the dealer separately
                                           (dealerMatchResults, playerMatchResults)  = (foldr (\thisResTuple res -> ((fst res) ++ [(fst thisResTuple)], (snd res) ++ [(snd thisResTuple)])  ) ([], [])  $ map (\thisPlayersHand -> whoWon dealerHand thisPlayersHand) playerHands) :: ([Result], [Result]);
                                           -- ^ ++ is slower but at least we don't have to reverse the list
                                           dealerScore = foldr (\thisResult total -> addResult total thisResult) mempty dealerMatchResults
                                           -- ^ the dealer's list of results is the mirror image of the players'
                                           } in  Just (dealerScore, playerMatchResults)


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
  where
    playerBusted playerHand = (let cards = fmap unwrapVisibility firstPlayerHand in isBust cards) :: Bool
    firstPlayerBusted = playerBusted firstPlayerHand
    secondPlayerBusted = playerBusted secondPlayerHand
    firstPlayerScore = handPoints $ fmap unwrapVisibility firstPlayerHand
    secondPlayerScore = handPoints $ fmap unwrapVisibility secondPlayerHand
  
  

