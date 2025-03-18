\subsection{Attack on Vigen\`ere Cipher}\label{sec:vignereattack}

\hide{
\begin{code}
module VigenereBreaker where

\end{code}
}

\hide{
\begin{code}

import Vigenere
import Data.Char
import Data.Time.Clock


-- Caesar decryption for a single letter given a shift (0-25).
caesarDecrypt :: Int -> Char -> Char
caesarDecrypt shift c
  | isAlpha c =
      let base = if isUpper c then ord 'A' else ord 'a'
      in chr $ base + mod (ord c - base - shift) 26
  | otherwise = c

-- Apply Caesar decryption to an entire string.
caesarDecryptStr :: Int -> String -> String
caesarDecryptStr shift = map (caesarDecrypt shift)

-- Expected English letter frequencies (in percentages for A-Z).
englishFrequencies :: [Double]
englishFrequencies = [8.167, 1.492, 2.782, 4.253, 12.702, 2.228,
                      2.015, 6.094, 6.966, 0.153, 0.772, 4.025,
                      2.406, 6.749, 7.507, 1.929, 0.095, 5.987,
                      6.327, 9.056, 2.758, 0.978, 2.360, 0.150,
                      1.974, 0.074]

-- Chi-squared statistic compares observed letter frequencies in text
-- with the expected frequencies of English.
chiSquared :: String -> Double
chiSquared text = sum [((fromIntegral obs - expected) ** 2) / expected
                         | (obs, expected) <- zip counts expectedCounts]
  where
    letters = [toUpper c | c <- text, isAlpha c]
    n = length letters
    counts = [count l letters | l <- ['A'..'Z']]
    count x xs = length (filter (== x) xs)
    expectedCounts = [p / 100 * fromIntegral n | p <- englishFrequencies]

-- For a block of text (each column of the cipher), try all Caesar shifts and pick the best (lowest chi-squared).
bestShift :: String -> Int
bestShift s = snd $ minimum [(chiSquared (caesarDecryptStr shift s), shift) | shift <- [0..25]]

-- Given a candidate key length, break the ciphertext into groups corresponding to each key letter.
groupColumns :: Int -> String -> [String]
groupColumns keyLen text =
  [ [ c | (i, c) <- zip [0..] text, i `mod` keyLen == j, isAlpha c ]
    | j <- [0 .. keyLen - 1] ]

--------------------------------------------------------------------------------
-- Attack 1: Friedman Test Attack
-- Estimate key length from the overall Index of Coincidence.

-- Calculate the index of coincidence for a text.
indexOfCoincidence :: String -> Double
indexOfCoincidence text =
  let letters = [toUpper c | c <- text, isAlpha c]
      n = length letters
      freqs = [count l letters | l <- ['A'..'Z']]
      count x xs = length (filter (== x) xs)
  in if n <= 1 
       then 0 
       else sum [ fromIntegral (f * (f - 1)) | f <- freqs ] / fromIntegral (n * (n - 1))


-- Estimate key length using Friedman formula.
friedmanEstimate :: String -> Int
friedmanEstimate text =
  let n = length [ c | c <- text, isAlpha c ]
      ic = indexOfCoincidence text
      k = (0.027 * fromIntegral n) / (((fromIntegral n - 1) * ic) - (0.038 * fromIntegral n) + 0.065)
  in max 1 (round k)

-- Friedman attack: estimate key length then solve for key using frequency analysis.
friedmanAttack :: String -> IO (String, String, NominalDiffTime)
friedmanAttack ciphertext = do
  start <- getCurrentTime
  let estKeyLen = friedmanEstimate ciphertext
      groups = groupColumns estKeyLen ciphertext
      key = map (\g -> chr (bestShift g + ord 'A')) groups
      decoded = vigenereDecode key ciphertext
  end <- getCurrentTime
  let timeTaken = diffUTCTime end start
  return (key, decoded, timeTaken)

--------------------------------------------------------------------------------
-- Attack 2: Kasiski Test Attack
-- Search for repeated trigrams and use the distances between repeats to guess key length.

-- Kasiski estimate: for each repeated three-letter sequence, record the distances.
kasiskiEstimate :: String -> Int
kasiskiEstimate ciphertext =
  let cleaned = [ toUpper c | c <- ciphertext, isAlpha c ]
      triples = [ (take 3 (drop i cleaned), i) | i <- [0 .. length cleaned - 3] ]
      repeated = [ (str, i, j)
                 | (str, i) <- triples
                 , (str', j) <- triples
                 , i < j, str == str' ]
      distances = [ j - i | (_, i, j) <- repeated ]
      candidates = [ (k, length [ d | d <- distances, d `mod` k == 0 ]) | k <- [2 .. 20] ]
      -- Pick the key length candidate with the highest count.
      bestCandidate = snd $ maximum [ (count, k) | (k, count) <- candidates ]
  in bestCandidate

-- Kasiski attack: estimate key length with Kasiski then recover key via frequency analysis.
kasiskiAttack :: String -> IO (String, String, NominalDiffTime)
kasiskiAttack ciphertext = do
  start <- getCurrentTime
  let estKeyLen = kasiskiEstimate ciphertext
      groups = groupColumns estKeyLen ciphertext
      key = map (\g -> chr (bestShift g + ord 'A')) groups
      decoded = vigenereDecode key ciphertext
  end <- getCurrentTime
  let timeTaken = diffUTCTime end start
  return (key, decoded, timeTaken)

--------------------------------------------------------------------------------
-- Attack 3: Brute Force Attack over key lengths 1 to 10.
-- For each candidate key length, solve for the key and score the plaintext.
bruteForceAttack :: String -> IO (String, String, NominalDiffTime)
bruteForceAttack ciphertext = do
  start <- getCurrentTime
  let candidates = [ let groups = groupColumns keyLen ciphertext
                         key = map (\g -> chr (bestShift g + ord 'A')) groups
                         decoded = vigenereDecode key ciphertext
                         score = chiSquared decoded
                     in (score, key, decoded)
                   | keyLen <- [1..10] ]
      -- OLD: (bestScore, bestKey, bestDecoded) = minimum candidates
  let (_, bestKey, bestDecoded) = minimum candidates
  end <- getCurrentTime
  let timeTaken = diffUTCTime end start
  return (bestKey, bestDecoded, timeTaken)

--------------------------------------------------------------------------------
-- Main function: Read input, run all three attacks, and output their results.

io :: IO ()
io = do
  putStrLn "\nEnter the key for the Vigenère cipher:"
  key <- getLine
  putStrLn "\nEnter the message to encode:"
  message <- getLine
  let encoded = vigenereEncode key message
  let decoded = vigenereDecode key encoded
  putStrLn $ "Encoded : " ++ encoded
  putStrLn $ "Decoded : " ++ decoded


attack :: IO ()
attack = do
  putStrLn "Enter the ciphertext (assumed to be Vigenère encrypted):"
  ciphertext <- getLine
  putStrLn "\nRunning Friedman Attack..."
  (key1, decoded1, time1) <- friedmanAttack ciphertext
  putStrLn $ "Friedman Attack Results:\nTime taken: " ++ show time1 ++
             "\nEstimated key: " ++ key1 ++ "\nDecoded text: " ++ decoded1 ++ "\n"
  putStrLn "Running Kasiski Attack..."
  (key2, decoded2, time2) <- kasiskiAttack ciphertext
  putStrLn $ "Kasiski Attack Results:\nTime taken: " ++ show time2 ++
             "\nEstimated key: " ++ key2 ++ "\nDecoded text: " ++ decoded2 ++ "\n"
  putStrLn "Running Brute Force Attack..."
  (key3, decoded3, time3) <- bruteForceAttack ciphertext
  putStrLn $ "Brute Force Attack Results:\nTime taken: " ++ show time3 ++
             "\nEstimated key: " ++ key3 ++ "\nDecoded text: " ++ decoded3 ++ "\n"

main :: IO ()
main = io

\end{code}
}