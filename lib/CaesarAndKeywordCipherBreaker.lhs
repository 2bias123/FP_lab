\subsection{Attack on Caesar and Keyword Cipher}\label{sec:substitutionattack}

\hide{

\begin{code}

module CaesarAndKeywordCipherBreaker where

import CaesarAndKeywordCipher

import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List ( nub, (\\) )
import Data.Char (isAlpha,  toUpper)

\end{code}
}

As the key size for the Caesar is only one, we can use a brute force attack to break the cipher. We iterate over all possible keys and decrypt the ciphertext with the given key. We then return the key and the plaintext. This funciton returns a list of tuples, where the first element is the key and the second element is the plaintext. 

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


