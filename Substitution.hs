module Substitution where

import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List ( nub, (\\) )
import Data.Char (isAlpha, isSpace, toUpper)

class CipherInput a where
    createCipherMap :: a -> M.Map Char Char

emptyMap :: M.Map Char Char
emptyMap = M.empty

instance CipherInput String where
    createCipherMap :: String -> M.Map Char Char
    createCipherMap keyword =
        let baseAlphabet = ['A'..'Z']
            uniqueKeyword = map toUpper . nub $ keyword
            remainingLetters = baseAlphabet \\ uniqueKeyword
            cipherAlphabet = uniqueKeyword ++ remainingLetters
        in M.fromList $ zip baseAlphabet cipherAlphabet


instance CipherInput Int where
    createCipherMap :: Int -> M.Map Char Char
    createCipherMap rotation =
        let baseAlphabet = ['A'..'Z']
            (firstPart, secondPart) = splitAt (rotation `mod` 26) baseAlphabet
            flippedAlphabet = secondPart ++ firstPart
        in M.fromList $ zip baseAlphabet flippedAlphabet

generateCipherTextFromMap :: String -> M.Map Char Char -> String
generateCipherTextFromMap plainTxt pairs =
    let filteredText = mapMaybe (\c -> if isAlpha c then M.lookup (toUpper c) pairs else Nothing) plainTxt
    in unwords (chunk 5 filteredText)

chunk :: Int -> String -> [String]
chunk _ [] = []
chunk n str = take n str : chunk n (drop n str)

encryptSubstitiution :: CipherInput p => p -> String -> String
encryptSubstitiution key plaintext =
    let cipherMap = createCipherMap key
    in generateCipherTextFromMap plaintext cipherMap

-- Invert a cipher map to create a decryption map
invertCipherMap :: M.Map Char Char -> M.Map Char Char
invertCipherMap = M.fromList . map (\(k, v) -> (v, k)) . M.toList

-- Process ciphertext for decryption (preserve spaces and punctuation)
decryptCipherText :: String -> M.Map Char Char -> String
decryptCipherText cipherTxt decryptMap =
    map (\c -> if isAlpha c
              then fromMaybe c (M.lookup (toUpper c) decryptMap)
              else c) cipherTxt

-- Main decryption function that works with both String and Int keys
decryptSubstitution :: CipherInput p => p -> String -> String
decryptSubstitution key ciphertext =
    let cipherMap = createCipherMap key
        decryptMap = invertCipherMap cipherMap
    in decryptCipherText ciphertext decryptMap


bruteforceSubstitution :: Int -> String -> [(Int, String)]
bruteforceSubstitution iteration ciphertext
    |iteration > 26 = []
    |otherwise =
        let plaintext = decryptSubstitution iteration ciphertext
        in (iteration, plaintext) : bruteforceSubstitution (iteration + 1) ciphertext

bruteforceCaesar :: String -> [(Int, String)]
bruteforceCaesar = bruteforceSubstitution 1

-- Common English words that might be used as keywords
commonKeywords :: [String]
commonKeywords = [
    "THE", "AND", "THAT", "HAVE", "FOR", "NOT", "WITH", "YOU", "THIS", "BUT",
    "THEY", "SAY", "HER", "SHE", "FOR", "WILL", "ONE", "ALL", "WOULD", "THERE",
    "THEIR", "WHAT", "SO", "UP", "OUT", "IF", "ABOUT", "WHO", "GET", "WHICH",
    "GO", "ME", "WHEN", "MAKE", "CAN", "LIKE", "TIME", "NO", "JUST", "HIM",
    "KNOW", "TAKE", "PEOPLE", "INTO", "YEAR", "YOUR", "GOOD", "SOME", "COULD",
    "THEM", "SEE", "OTHER", "THAN", "THEN", "NOW", "LOOK", "ONLY", "COME", "ITS",
    "OVER", "THINK", "ALSO", "BACK", "AFTER", "USE", "TWO", "HOW", "OUR", "WORK",
    "FIRST", "WELL", "WAY", "EVEN", "NEW", "WANT", "BECAUSE", "ANY", "THESE",
    "GIVE", "DAY", "MOST", "US"
    ]

-- Try to crack keyword-based substitution using common words
crackKeywordSubstitution :: String -> [(String, String)]
crackKeywordSubstitution ciphertext = 
    [(keyword, decryptSubstitution keyword ciphertext) | keyword <- commonKeywords]

-- Helper function to get character frequency in a string
charFrequency :: String -> [(Char, Int)]
charFrequency str = 
    let upperStr = map toUpper str
        alphaOnly = filter isAlpha upperStr
        counts = map (\c -> (c, length $ filter (==c) alphaOnly)) ['A'..'Z']
    in filter ((>0) . snd) counts

-- Try to crack using frequency analysis and common words
crackWithFrequency :: String -> [(String, String)]
crackWithFrequency ciphertext = 
    let freq = charFrequency ciphertext
        -- Sort keywords by how well they match the frequency pattern
        sortedKeywords = sortBy (compareFreq freq) commonKeywords
        -- Try top 20 most promising keywords
        topKeywords = take 20 sortedKeywords
    in [(keyword, decryptSubstitution keyword ciphertext) | keyword <- topKeywords]

-- Compare two keywords based on frequency pattern matching
compareFreq :: [(Char, Int)] -> String -> String -> Ordering
compareFreq freq k1 k2 = 
    let score1 = frequencyScore freq k1
        score2 = frequencyScore freq k2
    in compare score2 score1  -- Higher score comes first

-- Calculate how well a keyword matches the frequency pattern
frequencyScore :: [(Char, Int)] -> String -> Int
frequencyScore freq keyword = 
    let keywordFreq = charFrequency keyword
        commonChars = map fst $ take 5 $ sortBy (flip compare `on` snd) freq
        score = length $ filter (`elem` commonChars) keyword
    in score

