{-# LANGUAGE ExistentialQuantification #-}
module Jakway.Blackjack.Util where

innerMapTuple4 :: forall t t1. (t -> t1) -> (t, t, t, t) -> (t1, t1, t1, t1)
innerMapTuple4 f (a,b,c,d) = (f a, f b, f c, f d)

innerMapTuple3 :: forall t t1. (t -> t1) -> (t, t, t) -> (t1, t1, t1)
innerMapTuple3 f (a,b,c) = (f a, f b, f c)
