module Jakway.Blackjack.Match where

import Jakway.Blackjack.CardOps
import Jakway.Blackjack.Result

data Match = Match
            { dealersHand :: Hand
            , playerIds :: [Int]
            , playersHands :: [Hand]
            , playerResults :: [Result]
            }
