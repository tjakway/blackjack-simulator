module Jakway.Blackjack.Util where

innerMapTuple4 f (a,b,c,d) = (f a, f b, f c, f d)

innerMapTuple3 f (a,b,c) = (f a, f b, f c)