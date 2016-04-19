module Jakway.Blackjack.Fuzzy.Options
(
Config(..)
)
where

data Distribution = EvenDistribution | RNGDistribution

data Config = Config
            { distribution :: Distribution,
              pvalue :: Double,
              sampleSize :: Integer
            }

data Flag = WhichDistribution Distribution |
            PValue Double |
            SampleSize Integer
            deriving (Show, Eq)

getWhichDistribution :: Flag -> Maybe Distribution
getWhichDistribution EvenDistribution = Just EvenDistribution
getWhichDistribution RNGDistribution = Just RNGDistribution
getWhichDistribution _ = Nothing

getPValue :: Flag -> Maybe Double
getPValue (PValue val) = Just val
getPValue _ = Nothing

getSampleSize :: Flag -> Maybe Integer
getSampleSize (SampleSize n) = Just n
getSampleSize _ = Nothing

flagsToConfig :: [Flag] -> Either String Config
flagsToConfig [] = Left "No flags passed."
flagsToConfig flags
                | dist == Nothing = Left "No distribution passed."
                | pval < 0 = Left "P value must be greater than 0."
                | n < 104 = Left "Sample size must be greater than 104."
                | otherwise = Right . Config $ fromJust <$> dist <*> pval <*> n
        where dist = getSingleFlag getWhichDistribution flags
              pval = getSingleFlag getPValue flags
              n = getSingleFlag getSampleSize flags



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


--returns a valid configuration or prints an error and exits
getConfig :: [String] -> IO Conf.Config
getConfig argv = parseOptions argv options "Usage: blackjack-simulator [OPTION...]" >>= \(flags, _) -> return (flagsToConfig flags) >>=
                                \res -> case res of (Left x) -> die $ "Error processing options: " ++ x
                                                    (Right conf) -> return conf
