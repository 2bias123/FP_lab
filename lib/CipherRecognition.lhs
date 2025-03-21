\section{Cipher recognition}\label{sec:recognition}

This section is about cypher recognition.

\hide{
\begin{code}

module CipherRecognition where
import Data.Char ( isAlpha, toLower )
import qualified Data.Map as M
import Data.List (sortBy)
import Data.Ord (comparing)
import Control.DeepSeq (force)  -- For strict evaluation


-- Normalize text by keeping only alphabetic characters and converting to lowercase
normalizeText :: String -> String
normalizeText = map toLower . filter isAlpha

-- Count letter frequencies in a string
letterFrequencies :: String -> M.Map Char Int
letterFrequencies text =
    M.fromListWith (+) [(c, 1) | c <- text]

calculateIoC :: M.Map String Int -> Double
calculateIoC freqMap = 
    let n = sum $ M.elems freqMap                      -- Total character count
        numerator = sum $ M.map (\f -> f * (f - 1)) freqMap  -- Sum of f(f-1)
        denominator = n * (n - 1)                      -- Total possible pairs
    in if denominator == 0 
          then 0 
          else fromIntegral numerator / fromIntegral denominator


-- Count n-grams in text efficiently
countNgrams :: Int -> String -> M.Map String Double
countNgrams n text = M.fromListWith (+) $
    [(force ngram, 1.0) | i <- [0..length text - n], 
                        let ngram = take n (drop i text), 
                        length ngram == n]

-- Convert raw counts to probabilities
mapProbabilities :: M.Map String Double -> M.Map String Double
mapProbabilities ngramMap =
    let total = sum $ M.elems ngramMap
    in M.map (/ total) ngramMap

-- Convert to log probabilities for better numerical stability
logProbabilities :: M.Map String Double -> M.Map String Double
logProbabilities = M.map log

-- Sort frequency map to pairs of (char, frequency) in descending order
sortedPairs :: M.Map Char Int -> [(Char, Int)]
sortedPairs freqMap = sortBy (flip (comparing snd)) (M.toList freqMap)

-- Extract just the characters from sorted pairs
sortedChars :: [(Char, Int)] -> [Char]
sortedChars = map fst

\end{code}
}