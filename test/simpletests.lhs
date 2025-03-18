\section{Tests}
\label{sec:tests}

We now use the library QuickCheck to randomly generate input for the ciphers, and test whether they work correctly.
We can also use QuickCheck to test attacks maybe?

\hide{
\begin{code}

module Main where

{-
import Test.QuickCheck
import Vigenere
import Substitution
import Playfair

-- Test for Vigenere cipher
prop_vigenere :: String -> String -> Bool
prop_vigenere key message =
  let encoded = vigenereEncode key message
      decoded = vigenereDecode key encoded
  in decoded == message

-- Test for Substitution cipher
prop_substitution :: String -> String -> Bool
prop_substitution key message =
  let encoded = encryptSubstitiution key message
      decoded = decryptSubstitution key encoded
  in decoded == message

-- Test for Playfair cipher
prop_playfair :: String -> String -> Bool
prop_playfair key message =
  let encoded = encryptPlayfair key message
      decoded = decryptPlayfair key encoded
  in decoded == message

-- Main function to run all tests
main :: IO ()
main = do
  quickCheck prop_vigenere
  quickCheck prop_substitution
  quickCheck prop_playfair
  
-}
\end{code} 
}