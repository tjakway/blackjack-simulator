module Jakway.Blackjack.Fuzzy.Options

data Distribution = EvenDistribution | RNGDistribution

data Flag = WhichDistribution Distribution |
            PValue Double |
            SampleSize Integer
            deriving (Show, Eq)

whichDistOpt, pvalueOpt, sampleSizeOpt :: String -> Flag
whichDistOpt = WhichDistribution . read
pvalueOpt = PValue . read
sampleSizeOpt = SampleSize . read

options :: [OptDescr Flag]
options =
    [ Option ['p']        ["pvalue"]  (ReqArg pvalueOpt "percent")  "P value."
    , Option ['n']        ["sample-size"]  (ReqArg sampleSizeOpt  "num")  "Number of samples."
    , Option ['d']     ["distribution"]  (ReqArg  "distribution") "Which test to run (the expected distribution).  Possible values are EvenDistribution and RNGDistribution"
    ]

