\subsection{Attack on Playfair Cipher}\label{sec:playfairattack}

\hide{
\begin{code}
module PlayfairBreaker where

import Data.Char (toUpper, isAlpha)
import Data.List (nub, maximumBy)
import Data.Ord (comparing)

-- | The alphabet used for the Playfair table (I and J are treated as the same letter)
alphabet :: [Char]
alphabet = filter (/= 'J') ['A'..'Z']

-- | Create the Playfair table from a keyword.
createTable :: String -> [Char]
createTable keyword =
  let cleaned = map (\c -> if c == 'J' then 'I' else c) $
                filter isAlpha $ map toUpper keyword
      keyUnique = nub cleaned
  in keyUnique ++ filter (`notElem` keyUnique) alphabet

-- | Break a list into chunks of n elements.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Prepare text by converting to uppercase, removing non-letters, and replacing 'J' with 'I'
prepareText :: String -> String
prepareText = map (\c -> if c == 'J' then 'I' else c) . filter isAlpha . map toUpper

-- | Split text into digrams. Insert an 'X' between duplicate letters and pad if necessary.
makeDigrams :: String -> [String]
makeDigrams []       = []
makeDigrams [x]      = [[x, 'X']]
makeDigrams (x:y:xs)
  | x == y    = [x, 'X'] : makeDigrams (y:xs)
  | otherwise = [x, y]   : makeDigrams xs

-- | Find the (row, column) of a character in the 5x5 table.
findPosition :: [[Char]] -> Char -> (Int, Int)
findPosition table c =
  let positions = [ (r, col) 
                  | (r, row) <- zip [0..] table
                  , (col, ch) <- zip [0..] row
                  , ch == c ]
  in case positions of
       []    -> error ("Character not found: " ++ [c])
       (p:_) -> p

-- | Encrypt a single digram using Playfair rules.
encryptDigram :: [[Char]] -> String -> String
encryptDigram table [a,b] =
  let (rowA, colA) = findPosition table a
      (rowB, colB) = findPosition table b
  in if rowA == rowB then
       -- Same row: shift right (wrap around)
       [ table !! rowA !! ((colA + 1) `mod` 5)
       , table !! rowB !! ((colB + 1) `mod` 5) ]
     else if colA == colB then
       -- Same column: shift down (wrap around)
       [ table !! ((rowA + 1) `mod` 5) !! colA
       , table !! ((rowB + 1) `mod` 5) !! colB ]
     else
       -- Rectangle: swap columns.
       [ table !! rowA !! colB
       , table !! rowB !! colA ]
encryptDigram _ _ = error "Invalid digram length"

-- | Decrypt a single digram using reverse Playfair rules.
decryptDigram :: [[Char]] -> String -> String
decryptDigram table [a,b] =
  let (rowA, colA) = findPosition table a
      (rowB, colB) = findPosition table b
  in if rowA == rowB then
       -- Same row: shift left (wrap around)
       [ table !! rowA !! ((colA - 1) `mod` 5)
       , table !! rowB !! ((colB - 1) `mod` 5) ]
     else if colA == colB then
       -- Same column: shift up (wrap around)
       [ table !! ((rowA - 1) `mod` 5) !! colA
       , table !! ((rowB - 1) `mod` 5) !! colB ]
     else
       -- Rectangle: swap columns.
       [ table !! rowA !! colB
       , table !! rowB !! colA ]
decryptDigram _ _ = error "Invalid digram length"

-- | Encrypt an entire message using the Playfair cipher.
encrypt :: String -> String -> String
encrypt keyword text =
  let tableFlat = createTable keyword
      table     = chunksOf 5 tableFlat
      prepared  = prepareText text
      digrams   = makeDigrams prepared
  in concatMap (encryptDigram table) digrams

-- | Decrypt an entire ciphertext using the Playfair cipher.
decrypt :: String -> String -> String
decrypt keyword text =
  let tableFlat = createTable keyword
      table     = chunksOf 5 tableFlat
      digrams   = chunksOf 2 (prepareText text)
  in concatMap (decryptDigram table) digrams

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
  let candidateDecryptions = [ (kw, decrypt kw cipherText) | kw <- keys ]
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