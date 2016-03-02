module Jakway.Blackjack.Invariants.Checks where

import Jakway.Blackjack.CardOps
import Jakway.Blackjack.Visibility

checkHand :: Hand -> Either String Bool
checkHand hand = numHiddenCheck >>= (\b -> Right True)
        where numHidden = foldr (\thisCard hiddenSum -> case thisCard
                                of (Hidden _) -> hiddenSum + 1
                                   (Shown _) -> hiddenSum) 0 hand
              numHiddenCheck = if numHidden /= 1 then Left "Should only have 1 face down card" else Right True
