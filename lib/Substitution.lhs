\subsection{Substitution cipher}\label{sec:vignere}

Explain code of cipher implementation in codeblocks, unhide important things from below

\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Substitution where
\end{code}

\hide{
\begin{code}


import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List ( nub, (\\) )
import Data.Char (isAlpha,  toUpper)

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

\end{code}
}