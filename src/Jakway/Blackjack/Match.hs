module Jakway.Blackjack.Match where

data Result = Result
            { dealersHand :: Hand
            , playersHands :: [Hand]
            , playerIds :: [Int]
            , playerResults :: [Result]
            }
