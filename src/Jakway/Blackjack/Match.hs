module Jakway.Blackjack.Match where

data Match = Match
            { dealersHand :: Hand
            , playersHands :: [Hand]
            , playerIds :: [Int]
            , playerResults :: [Result]
            }
