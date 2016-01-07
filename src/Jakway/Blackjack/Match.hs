module Jakway.Blackjack.Match where

data Match = Match
            { dealersHand :: Hand
            , playerIds :: [Int]
            , playersHands :: [Hand]
            , playerResults :: [Result]
            }
