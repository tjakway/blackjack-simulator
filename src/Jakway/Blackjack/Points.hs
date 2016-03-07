module Jakway.Blackjack.Points where

import Jakway.Blackjack.Cards
import Jakway.Blackjack.CardOps
import Data.Monoid

cardPoints :: CardValue -> Int
cardPoints cardVal 
  | cardVal == Jack  = 10
  | cardVal == Queen = 10
  | cardVal == King  = 10
  --count aces as 11 now, can decrement 10 later as necessary
  | cardVal == Ace   = 11
  --enums count up from 0 but the first card type is 2
  | otherwise = 2 + fromEnum cardVal

handPoints :: [Card] -> Int
handPoints hand
                | total <= 21 = total 
                | total > 21 && hasAce = total - 10
                | otherwise = total
            where hasAce = hasCard hand Ace
                  total = getSum $ foldMap (Sum . cardPoints . cardValue) hand 

isBust :: [Card] -> Bool
isBust hand = 21 < handPoints hand
