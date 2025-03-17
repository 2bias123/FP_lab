module CipherRecognition where
import Data.Char ( isAlpha, toLower )
import qualified Data.Map as M

normalizeText :: String -> String
normalizeText = map toLower . filter isAlpha

letterFrequencies :: String -> M.Map Char Double
letterFrequencies text =
    let total = fromIntegral (length text)
        counts = M.fromListWith (+) [(c, 1) | c <- text]
    in counts


calculateIoC :: M.Map Char Int -> Double
calculateIoC freqMap = 
    let n = sum $ M.elems freqMap                      -- Total character count
        numerator = sum $ M.map (\f -> f * (f - 1)) freqMap  -- Sum of f(f-1)
        denominator = n * (n - 1)                      -- Total possible pairs
    in if denominator == 0 
          then 0 
          else fromIntegral numerator / fromIntegral denominator


-- extractNgrams :: Int -> String -> M.Map String Double
-- extractNgrams n = 