\subsection{Vigen\`ere Cipher}\label{sec:vignere}
\begin{code}
module Vigenere where
\end{code}

\hide{
\begin{code}
import Data.Char
import Data.Time.Clock
\end{code}
}

Unlike a simple monoalphabetic shift, the Vigen\`ere cipher uses multiple shifts determined by a repeating keyword. In Haskell, this design naturally translates into a few elegant higher-order functions, list manipulations, and local bindings.

We define \texttt{vigenere}, which takes a higher order function \texttt{op}, a \texttt{key} and a plaintext, and returns the ciphertext. We rely on \texttt{cycle} to stream each character of the \texttt{key} infinitely, then pair it with each plaintext character. The recursive helper \texttt{enc} uses pattern matching to either shift a character or leave them as-is, depending on whether it is an alphabetical letter - this is an elegant way of replacing manual loop constructs.

\begin{code}
vigenere :: (Int -> Int -> Int) -> String -> String -> String
vigenere op key = enc (cycle key)
  where
    enc _ [] = []
    enc [] _ = []
    enc (k:ks) (t:ts)
      | isAlpha t = shift k t : enc ks ts
      | otherwise = t : enc (k:ks) ts
\end{code}

Here, \texttt{shift} calculates the offset by converting the key character \texttt{k} to uppercase and subtracting \texttt{'A'}. We conditionally decide the base code point (uppercase or lowercase) for the plaintext letter and then wrap around with modular arithmetic. Because all values are passed immutably, we avoid side effects and can directly transform each character.

\begin{code}
    shift k t =
      let base   = if isUpper t then ord 'A' else ord 'a'
          offset = ord (toUpper k) - ord 'A'
      in chr $ base + mod ((ord t - base) `op` offset) 26
\end{code}
By passing an operator (\texttt{\char`+} for encryption or \texttt{\char`-} for decryption)
 we generalized our cipher logic into one function, therefore encoding and decoding are defined as specializations of \texttt{vigenere} with the appropriate operator:

\begin{code}
vigenereEncode :: String -> String -> String
vigenereEncode key text = vigenere (+) key text

vigenereDecode :: String -> String -> String
vigenereDecode key text = vigenere (-) key text
\end{code}

Since each transformation is defined as a pure function, data flows in one direction without side effects. The pattern matching in \texttt{enc} clarifies when to apply shifts (only on alphabetic characters) and when to keep a character intact.


\hide{
\begin{code}

main :: IO ()
main = putStrLn "Vigenere!"

\end{code}
}