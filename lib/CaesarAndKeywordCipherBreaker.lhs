\subsection{Attack on Caesar and Keyword Cipher}\label{sec:substitutionattack}

\hide{

\begin{code}

module CaesarAndKeywordCipherBreaker where

import CaesarAndKeywordCipher
import CipherRecognition

import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List ( nub, (\\) )
import Data.Char (isAlpha,  toUpper, toLower, isAlphaNum)
import Control.Monad (replicateM)
import System.Random (randomRIO)
import Data.List (sortBy)
import Data.Ord (comparing)


\end{code}
}

The Caesar cipher has a key size of only one, making it vulnerable to a brute force attack. This approach involves iterating through all possible keys (1-26) and decrypting the ciphertext with each key. The function returns a list of tuples, where the first element is the key and the second element is the corresponding plaintext. This exhaustive approach is feasible due to the limited keyspace of the Caesar cipher.

\begin{code}
bruteforceSubstitution :: Int -> String -> [(Int, String)]
bruteforceSubstitution iteration ciphertext
    |iteration > 26 = []
    |otherwise =
        let plaintext = decryptCaesarAndKeyword iteration ciphertext
        in (iteration, plaintext) : bruteforceSubstitution (iteration + 1) ciphertext

bruteforceCaesar :: String -> [(Int, String)]
bruteforceCaesar = bruteforceSubstitution 1
\end{code}

The keyword cipher is more complex to break, as the key size is variable. In order to break this cipher the fact that the properties of the natural language is preserved in the ciphertext of the keyword cipher. In any given stretch of written language, certain letters and combinations of letters occur with differing frequencies. In addition to this, a characteristic distribution of letter frequency in the given language will be almost the same for every sample of that language. These properties can be exploited to reqonstruct the alpahbet used for encryption.

% \begin{code}
% letterFrequencies :: String -> M.Map Char Double
% letterFrequencies text =
%     let counts = M.fromListWith (+) [(c, 1) | c <- text]
%     in counts
% \end{code}

The \texttt{letterFrequencies} function counts the occurrences of each letter in the input text and returns a map where the keys are the letters and the values are the counts. These values can be used to find the most comon letters in the ciphertext, and map these to the most common letters in the English language. 



------

\begin{code}

-- Basic constants and alphabet
alphabet :: [Char]
alphabet = ['a'..'z']

-- Pad a character list with remaining alphabet letters
padToFullAlphabet :: [Char] -> [Char]
padToFullAlphabet chars = chars ++ (alphabet \\ chars)

-- Generate initial mapping based on frequency analysis
generateInitialCrackMap :: String -> M.Map Char Char
generateInitialCrackMap text = 
    M.fromList $ zip sortedFreqChars frequencyAlphabet
  where
    freqMap = letterFrequencies text
    sortedFreqChars = padToFullAlphabet $ sortedChars $ sortedPairs freqMap
    frequencyAlphabet = "etaoinshrdlucmfpgwybvkxjqz"  -- Standard English frequency order


-- Score text using trigram model - optimized version
scoreTrigrams :: M.Map String Double -> String -> Double
scoreTrigrams trigramMap text =
    -- Sum up found trigrams, with a default penalty for unknown trigrams
    sum [M.findWithDefault defaultPenalty trigram trigramMap | 
         i <- [0 .. length cleaned - 3], 
         let trigram = take 3 (drop i cleaned)]
  where
    cleaned = normalizeText text
    -- Default penalty for unknown trigrams (should be less than minimum log probability)
    defaultPenalty = -20.0  -- Adjust this value based on your dataset

-- Swap two random letters in the key map
swapTwoLetters :: M.Map Char Char -> IO (M.Map Char Char)
swapTwoLetters m = do
    a <- randomRIO ('a', 'z')
    b <- randomRIO ('a', 'z')
    if a == b
        then swapTwoLetters m  -- retry until different
        else do
            -- Find which cipher letters map to a and b
            let revMap = M.fromList [(v, k) | (k, v) <- M.toList m]
                keyA = M.findWithDefault a a revMap
                keyB = M.findWithDefault b b revMap
            -- Swap the mappings
            return $ M.insert keyA b $ M.insert keyB a m

-- Hill climbing optimization with early stopping and temperature control
hillClimb :: Int -> String -> M.Map String Double -> M.Map Char Char -> 
            IO (M.Map Char Char, String, Double)
hillClimb maxIter cipher logTrigrams initialKey = do
    let initialPlain = decryptCipherText cipher initialKey
        initialScore = scoreTrigrams logTrigrams initialPlain
    
    -- Use a helper function with an accumulator to track best result
    go maxIter initialKey initialPlain initialScore initialKey initialPlain initialScore 0
  where
    go 0 _ _ _ bestKey bestPlain bestScore _ = 
        return (bestKey, bestPlain, bestScore)
    
    go n currKey currPlain currScore bestKey bestPlain bestScore unchanged
        -- Early stopping if no improvement for many iterations
        | unchanged > 1000 = return (bestKey, bestPlain, bestScore)
        | otherwise = do
            -- Try a new key
            newKey <- swapTwoLetters currKey
            let newPlain = decryptCipherText cipher newKey
                newScore = scoreTrigrams logTrigrams newPlain
            
            -- Determine whether to accept the new key
            if newScore > currScore
                then 
                    -- Accept improvement
                    let (newBestKey, newBestPlain, newBestScore, newUnchanged) = 
                            if newScore > bestScore
                                then (newKey, newPlain, newScore, 0)  -- New best found
                                else (bestKey, bestPlain, bestScore, unchanged + 1)
                    in go (n-1) newKey newPlain newScore newBestKey newBestPlain newBestScore newUnchanged
                else
                    -- Reject the change
                    go (n-1) currKey currPlain currScore bestKey bestPlain bestScore (unchanged + 1)

-- Process a text file and build a trigram model efficiently
buildTrigramModel :: FilePath -> IO (M.Map String Double)
buildTrigramModel filePath = do
    content <- readFile filePath
    -- Process the file in smaller chunks to avoid memory issues
    let normalized = normalizeText content
        -- Process in chunks of reasonable size
        chunks = chunksOf 10000 normalized
        -- Count trigrams in each chunk
        chunkCounts = map (countNgrams 3) chunks
        -- Combine all counts
        combined = foldl (M.unionWith (+)) M.empty chunkCounts
    -- Convert to log probabilities
    return $ logProbabilities $ mapProbabilities combined
  where
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Main function
main :: IO ()
main = do
    putStrLn "Loading language model from War and Peace..."
    trigramProbs <- buildTrigramModel "lib/WarAndPeace.txt"
    
    putStrLn "Reading cipher text..."
    ciphertext <- readFile "lib/cipher.txt"
    let normalized = normalizeText ciphertext
    
    putStrLn "Generating initial mapping based on frequency analysis..."
    let initKey = generateInitialCrackMap normalized
    
    putStrLn "Starting hill climbing optimization..."
    (bestKey, decrypted, score) <- hillClimb 1000000 normalized trigramProbs initKey
    
    putStrLn "\nBest Decryption:"
    putStrLn decrypted
    putStrLn $ "\nScore: " ++ show score
    
    -- Print the mapping for analysis
    putStrLn "\nMapping (cipher -> plain):"
    let sortedMapping = sortBy (comparing fst) $ M.toList bestKey
    mapM_ (\(c, p) -> putStrLn $ [c] ++ " -> " ++ [p]) sortedMapping

\end{code}