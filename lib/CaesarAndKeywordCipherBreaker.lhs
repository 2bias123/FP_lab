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

\begin{code}
letterFrequencies :: String -> M.Map Char Double
letterFrequencies text =
    let counts = M.fromListWith (+) [(c, 1) | c <- text]
    in counts
\end{code}

The \texttt{letterFrequencies} function counts the occurrences of each letter in the input text and returns a map where the keys are the letters and the values are the counts. These values can be used to find the most comon letters in the ciphertext, and map these to the most common letters in the English language. 
