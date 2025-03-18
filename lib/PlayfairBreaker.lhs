\subsection{Attack on Playfair Cipher}\label{sec:playfairattack}

\hide{
\begin{code}
module PlayfairBreaker where

import Playfair
import Data.Char (toUpper, isAlpha)
import Data.List (nub, maximumBy)
import Data.Ord (comparing)

--------------------------------------------------------------------------------
-- Brute Force Approach Using a List of Candidate Keywords
--------------------------------------------------------------------------------

-- | A list of common English bigrams that typically appear in English text.
commonBigrams :: [String]
commonBigrams = ["TH", "HE", "IN", "ER", "AN", "RE", "ND", "AT", "ON", "EN"]

-- | Count the occurrences of a substring in a string.
countOccurrences :: String -> String -> Int
countOccurrences sub str = length $ filter (sub `isPrefixOf`) (tails str)
  where
    isPrefixOf p s = take (length p) s == p
    tails [] = []
    tails xs@(_:xs') = xs : tails xs'

-- | Score a decrypted text by summing the occurrences of common bigrams.
scoreText :: String -> Int
scoreText text = sum [ countOccurrences bigram text | bigram <- commonBigrams ]

-- | Try all candidate keywords (from a list) and choose the decryption with the best score.
bruteForceDecrypt :: [String] -> String -> (String, String, Int)
bruteForceDecrypt keys cipherText =
  let candidateDecryptions = [ (kw, decryptPlayfair kw cipherText) | kw <- keys ]
      scored = [ (kw, plain, scoreText plain) | (kw, plain) <- candidateDecryptions ]
      -- We assume the candidate with the highest score is most likely correct.
      bestCandidate = maximumBy (comparing (\(_,_,score) -> score)) scored
  in bestCandidate

--------------------------------------------------------------------------------
-- Main Program: Reads candidate keys and ciphertext, then attempts brute force.
--------------------------------------------------------------------------------

main :: IO ()
main = do
  -- Read the file containing the candidate keywords (one per line)
  candidateContent <- readFile "common_words.txt"
  let candidateKeys = lines candidateContent
  putStrLn "Enter the ciphertext (letters only, I/J combined):"
  cipherText <- getLine
  let (bestKey, bestPlain, bestScore) = bruteForceDecrypt candidateKeys cipherText
  putStrLn "\nBrute force results:"
  putStrLn $ "Candidate key: " ++ bestKey
  putStrLn $ "Decrypted text: " ++ bestPlain
  putStrLn $ "Score: " ++ show bestScore

\end{code}
}