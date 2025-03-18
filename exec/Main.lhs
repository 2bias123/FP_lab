
\section{Running the program}\label{sec:main}

This section (our main file) provides an interface for encoding and decoding messages using different cipher techniques, including Vigenère, Substitution, and Playfair ciphers. The program will prompt the user to enter a key and a message, then display the encoded and decoded results for each cipher method.


\begin{code}
module Main where

import Vigenere
import Substitution
import Playfair

import VigenereBreaker
import PlayfairBreaker
import SubstitutionBreaker


main :: IO ()
main = do
  putStrLn "\nEnter the key for the Vigenere cipher:"
  key <- getLine
  putStrLn "\nEnter the message to encode:"
  message <- getLine
  let substitutionEncoded = encryptSubstitiution key message
  let substitutionDecoded = decryptSubstitution key substitutionEncoded

  let vignereEncoded = vigenereEncode key message
  let vignereDecoded = vigenereDecode key vignereEncoded

  let playfairEncoded = encryptPlayfair key message
  let playfairDecoded = decryptPlayfair key playfairEncoded

  putStrLn $ "\nSubstitution encoded : " ++ substitutionEncoded
  putStrLn $ "Substitution decoded : " ++ substitutionDecoded ++ "\n"

  putStrLn $ "Vignere encoded : " ++ vignereEncoded
  putStrLn $ "Vignere decoded : " ++ vignereDecoded ++ "\n"

  putStrLn $ "Playfair encoded : " ++ playfairEncoded
  putStrLn $ "Playfair decoded : " ++ playfairDecoded ++ "\n"

\end{code}