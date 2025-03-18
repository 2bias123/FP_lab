\subsection{Caesar and Keyword cipher}\label{sec:vignere}

\hide{
\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module CaesarAndKeywordCipher where

import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List ( nub, (\\) )
import Data.Char (isAlpha,  toUpper)

emptyMap :: M.Map Char Char
emptyMap = M.empty
\end{code}
}

The substitution cipher is a way of encrypting where the units of the plaintext is replaced with cipher text. There are several different kinds of substitution ciphers, but the one considered in this section is the Caesar and Keyword cipher. These ciphers are versions of the substitution cipher that works on single letters. The caesar cipher takes a int as key value, and the keyword cipher generates a scrambled alphabet based on a keyword. To accommodate different cipher key formats in a uniform and maintainable way the typeclass CipherInput is used. The CipherInput typeclass specifies that any type used as a cipher key must implement the create function. This allows for treating multiple cipher types uniformly, making use of Haskell's polymorphism. 

\begin{code}
class CipherInput a where
    createCipherMap :: a -> M.Map Char Char
\end{code}

The caesar cipher is constructed by writing the alphabet twice, once as the normal alphabet and once with the letters shifted by a number of steps or even reversed. To create this mapping a concrete implementation of the earlier defined CipherInput typeclass for the input type Int. This instance allows integers to serve as keys for the caesar cipher. The function generates a cipher map by splitting the regular alphabet at the rotation point, and then recombining the two segments into a shifted alphabet. The shifted alphabet is then zipped with the regular alphabet, forming the mapping between the plaintext letters and the cipher letters. 

\begin{code}
instance CipherInput Int where
    createCipherMap :: Int -> M.Map Char Char
    createCipherMap rotation =
        let baseAlphabet = ['A'..'Z']
            (firstPart, secondPart) = splitAt (rotation `mod` 26) baseAlphabet
            flippedAlphabet = secondPart ++ firstPart
        in M.fromList $ zip baseAlphabet flippedAlphabet
\end{code}

The keyword cipher is constructed by reordering the alphabet based on a given keyword, ensuring that each letter appears only once while preserving the remaining order of the alphabet. To create this mapping, a concrete implementation of the earlier defined CipherInput typeclass is provided for the input type String. This instance allows strings to serve as keys for the keyword cipher. The function generates a cipher map by extracting unique letters from the keyword, appending the remaining letters of the alphabet in order, and then zipping this modified sequence with the regular alphabet to form the mapping between plaintext and cipher letters.

\begin{code}
instance CipherInput String where
    createCipherMap :: String -> M.Map Char Char
    createCipherMap keyword =
        let baseAlphabet = ['A'..'Z']
            uniqueKeyword = map toUpper . nub $ keyword
            remainingLetters = baseAlphabet \\ uniqueKeyword
            cipherAlphabet = uniqueKeyword ++ remainingLetters
        in M.fromList $ zip baseAlphabet cipherAlphabet
\end{code}

The \texttt{generateCipherTextFromMap} function transforms plaintext using a cipher mapping by filtering out non-alphabetic characters and replacing valid ones through \texttt{M.lookup}, which returns a \texttt{Maybe Char}. The use of \texttt{mapMaybe} ensures only successful lookups (\texttt{Just} values) are kept, while non-alphabetic or unmapped characters are discarded. The encrypted output is then formatted into five-character chunks omitting punctuation and spaces using a recursive \texttt{chunk} function. This is done in order to disguise word boundaries from the plaintext as well as to help avoid transmission errors.

The \texttt{encryptCaesarAndKeyword} function orchestrates this by generating the cipher map with \texttt{createCipherMap} and passing it to \texttt{generateCipherTextFromMap}. Haskell's \texttt{Maybe} type makes this implementation safe by forcing explicit handling of missing values, preventing common lookup errors found in imperative languages.

\begin{code}
generateCipherTextFromMap :: String -> M.Map Char Char -> String
generateCipherTextFromMap plainTxt pairs =
    let filteredText = mapMaybe (\c -> if isAlpha c then M.lookup (toUpper c) pairs else Nothing) plainTxt
    in unwords (chunk 5 filteredText)

chunk :: Int -> String -> [String]
chunk _ [] = []
chunk n str = take n str : chunk n (drop n str)

encryptCaesarAndKeyword :: CipherInput p => p -> String -> String
encryptCaesarAndKeyword key plaintext =
    let cipherMap = createCipherMap key
    in generateCipherTextFromMap plaintext cipherMap
\end{code}

The decryption process of both the caesar and keyword cipher is straightforward, as it involves inverting the cipher map used for encryption. This is exactly what happens in the functions below. The main decryption function, \texttt{decryptCaesarAndKeyword}, first generates the key map using \texttt{createCipherMap}, then inverts it using \texttt{invertCipherMap}, which swaps the keys and values by converting the map to a list of pairs with \texttt{M.toList}, mapping each \texttt{(k, v)} pair to \texttt{(v, k)}, and reconstructing a new map with \texttt{M.fromList}. This functional approach is both concise and efficient because it avoids explicit loops and operates in a single pass over the map structure. The \texttt{decryptCipherText} function then applies the decryption map while preserving spaces and punctuation, using \texttt{map} to iterate over the text and \texttt{fromMaybe} to handle unmapped characters safely. This ensures that decryption is expressive and declarative, leveraging Haskell's built-in higher-order functions and persistent data structures to process transformations in a clear and optimized manner without unnecessary state mutations.

\begin{code}
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
decryptCaesarAndKeyword :: CipherInput p => p -> String -> String
decryptCaesarAndKeyword key ciphertext =
    let cipherMap = createCipherMap key
        decryptMap = invertCipherMap cipherMap
    in decryptCipherText ciphertext decryptMap
\end{code}
