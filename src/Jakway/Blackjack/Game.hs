{-# LANGUAGE ExistentialQuantification #-}
module Jakway.Blackjack.Game where

import Jakway.Blackjack.Cards
import Jakway.Blackjack.CardOps
import Jakway.Blackjack.AI
import Jakway.Blackjack.Result
import Jakway.Blackjack.Visibility
import Jakway.Blackjack.Points
import Jakway.Blackjack.Match
import Control.Monad.State
import Data.Ord
import Data.Monoid
import Data.Maybe (fromJust)
import System.Random

blackjack :: [Card] -> Bool
blackjack hand = 2 == length hand && hasAce && hasFaceCard
  where
    handHas = hasCard hand
    hasAce = handHas Ace
    faceCards = Card <$> allSuits <*> [Jack, Queen, King]
    hasFaceCard = any (handHas . cardValue) faceCards
    
startingHand :: Blackjack Hand
startingHand = do
  firstCard <- Hidden <$> drawCard
  secondCard <- Shown <$> drawCard
  return [firstCard, secondCard]

foldFnSt ai = startingHand >>= play' ai

-- |Plays a game and returns the relevant state
-- |returns a tuple of (dealersHand, playerHands, playerResults)
-- |dealer score record is redundant and not returned (just the opposite of
-- every Result)
evalGame :: AI -> [AI] -> Deck -> Maybe (Match)
evalGame dealerAI [] deck = Nothing
evalGame dealerAI allPlayers deck = flip evalState deck $ do
  dealersStartingHand <- startingHand
  playerHands <- reverse <$> mapM foldFnSt allPlayers
  dealerHand <- play' dealerAI dealersStartingHand
  let results = reverse . map (whoWon' dealerHand)
  return . Just $ Match dealerHand playerIDs playerHands (results playerHands)
  where playerIDs = [1.. (length allPlayers)]

infixl 8 &&&
(&&&) :: forall t t1 t2. (t2 -> t) -> (t2 -> t1) -> t2 -> (t, t1)
(f &&& g) a = (f a, g a)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, b) = (f a, b)

both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b)

play' :: AI -> Hand -> Blackjack Hand
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
whoWon' firstHand secondHand = ordToResult $ bust <> scores
  where
    busted = isBust . map unwrapVisibility 
    bust = comparing busted firstHand secondHand
    scores = comparing (handPoints . map unwrapVisibility) firstHand secondHand

--TODO: instead of making this tail recursive could just have it
--return Match instead of [Match] then use something from
--Data.List to repeat the function call and concat
genMatch dealerAI playerAIs  = do
    --parameterizing each game with a new deck is the best
    --approach because otherwise partial decks might be reused
    --(and there's no way to tell when a deck has been reused)
    --would be a subtle source of bias
    deck <- liftM infiniteShuffledDeck $ newStdGen
    return . fromJust $ evalGame dealerAI playerAIs deck
