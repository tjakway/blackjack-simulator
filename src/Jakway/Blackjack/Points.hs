module Jakway.Blackjack.Points where

import Jakway.Blackjack.Cards

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
