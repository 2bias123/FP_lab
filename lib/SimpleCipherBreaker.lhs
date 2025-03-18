\subsection{Attack on Substitution Cipher}\label{sec:substitutionattack}

\hide{

\begin{code}

module SimpleCipherBreaker where

import SimpleCipher
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List ( nub, (\\) )
import Data.Char (isAlpha,  toUpper)

bruteforceSubstitution :: Int -> String -> [(Int, String)]
bruteforceSubstitution iteration ciphertext
    |iteration > 26 = []
    |otherwise =
        let plaintext = decryptSimpleCipher iteration ciphertext
        in (iteration, plaintext) : bruteforceSubstitution (iteration + 1) ciphertext

bruteforceCaesar :: String -> [(Int, String)]
bruteforceCaesar = bruteforceSubstitution 1

\end{code}
}