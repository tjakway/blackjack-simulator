module Jakway.Blackjack.Invariants.Checks where

import Jakway.Blackjack.CardOps
import Jakway.Blackjack.Visibility

checkHand :: Hand -> Either String Bool
checkHand hand = hiddenCardsCheck hand

hiddenCardsCheck :: Hand -> Either String Bool
hiddenCardsCheck hand = numHiddenCheck >>= (\b -> Right True)

        where numHidden = foldr (\thisCard hiddenSum -> case thisCard
                                of (Hidden _) -> hiddenSum + 1
                                   (Shown _) -> hiddenSum) 0 hand
              numHiddenCheck = if numHidden /= 1 then Left "Should only have 1 face down card" else Right True

--invariants to implement:
-- * if both players bust it should be a tie
-- * no player should be over 30 (because it would mean they busted and
-- then hit again, which should never happen)


