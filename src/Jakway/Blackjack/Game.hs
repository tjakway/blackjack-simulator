module Jakway.Blackjack.Game where

import Jakway.Blackjack.Cards
import Jakway.Blackjack.AI
import Jakway.Blackjack.Result
import Jakway.Blackjack.Visibility
import Jakway.Blackjack.Points
import Control.Monad.State

blackjack :: [Card] -> Bool
blackjack hand = 2 == length hand && hasAce && hasFaceCard
  where
    handHas = hasCard hand
    hasAce = handHas Ace
    faceCards = Card <$> allSuits <*> [Jack, Queen, King]
    hasFaceCard = any (handHas . cardValue) faceCards
    
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
    dealerScore = foldr (flip addResult . oppositeResult) mempty
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
