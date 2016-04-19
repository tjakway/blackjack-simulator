module Jakway.Blackjack.Random.Options
(
Config(..),
Distribution(..),
getConfig
)
where

import System.Console.GetOpt
import Data.Maybe (fromJust)
import Jakway.Blackjack.Util


data Distribution = EvenDistribution |
                    RNGDistribution
                    deriving (Show, Eq, Read)

data Config = Config
            { distribution :: Distribution,
              pvalue :: Double,
              sampleSize :: Integer,
              rngSampleSize :: Integer
            }

data Flag = WhichDistribution Distribution |
            PValue Double |
            SampleSize Integer |
            RNGSampleSize Integer
            deriving (Show, Eq)

getWhichDistribution :: Flag -> Maybe Distribution
getWhichDistribution (WhichDistribution a) = Just a
getWhichDistribution _ = Nothing

getPValue :: Flag -> Maybe Double
getPValue (PValue val) = Just val
getPValue _ = Nothing

getSampleSize :: Flag -> Maybe Integer
getSampleSize (SampleSize n) = Just n
getSampleSize _ = Nothing

getRNGSampleSize :: Flag -> Maybe Integer
getRNGSampleSize (RNGSampleSize n) = Just n
getRNGSampleSize _ = Nothing

flagsToConfig :: [Flag] -> Either String Config
flagsToConfig [] = Left "No flags passed."
flagsToConfig flags
                | dist == Nothing = Left "No distribution passed."
                | pval < 0 = Left "P value must be greater than 0."
                | n <= 104 = Left "Sample size must be greater than 104."
                | ((== RNGDistribution) . fromJust $ dist) && (rngSampleSize == Nothing) = Left "Must specify RNG sample size."
                | otherwise = Right  (Config (fromJust dist) pval n (fromJust rngSampleSize))
        where dist = getSingleFlag getWhichDistribution flags
              pval = fromJust $ getSingleFlag getPValue flags
              n = fromJust $ getSingleFlag getSampleSize flags
              rngSampleSize = getSingleFlag getRNGSampleSize flags



whichDistOpt, pvalueOpt, sampleSizeOpt, rngSampleSizeOpt :: String -> Flag
whichDistOpt = WhichDistribution . read
pvalueOpt = PValue . read
sampleSizeOpt = SampleSize . read
rngSampleSizeOpt = RNGSampleSize . read

options :: [OptDescr Flag]
options =
    [ Option ['p']        ["pvalue"]  (ReqArg pvalueOpt "percent")  "P value."
    , Option ['n']        ["sample-size"]  (ReqArg sampleSizeOpt  "num")  "Number of samples."
    , Option ['d']     ["distribution"]  (ReqArg  whichDistOpt "distribution") "Which test to run (the expected distribution).  Possible values are EvenDistribution and RNGDistribution"
    , Option ['r']        ["rng-sample-size"] (ReqArg rngSampleSizeOpt "num") "Number of samples to take from the random number generator (should be at least several times the range)."
    ]


--returns a valid configuration or prints an error and exits
getConfig :: [String] -> IO Config
getConfig argv = parseOptions argv options "Usage: probability-testing [OPTION...]" >>= \(flags, _) -> return (flagsToConfig flags) >>=
                                \res -> case res of (Left x) -> die $ "Error processing options: " ++ x
                                                    (Right conf) -> return conf
