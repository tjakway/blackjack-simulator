module Jakway.Blackjack.Interface.Options where

import qualified System.Console.GetOpt.Simple as Opt

options = 

let options = makeOptions [ (noArg, "verbose", Optional,   "Be verbose."),
                            (noArg,   "with-BasicDealer",    Optional,   "Use BasicDealer AI."),
                            (arg,   "num-BasicPlayer",    Optional,   "Number of BasicPlayer AI's."),
                            (arg,   "num-games", Required, "Number of games to run.")
                           ]

(opts, args) <- getOptsArgs (makeOptions options) ["conf", "section"] ["command"]
