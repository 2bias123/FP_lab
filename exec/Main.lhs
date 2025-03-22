
\section{Running the program}\label{sec:main}

This section (our main file) provides an interface for encoding and decoding messages using different cipher techniques, including Vigenère, Substitution, and Playfair ciphers. The program will prompt the user to enter a key and a message, then display the encoded and decoded results for each cipher method.

\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Vigenere
import CaesarAndKeywordCipher 
import Playfair


import System.IO (hFlush, stdout)
import Data.Char (toLower)
import Data.List (isInfixOf)
import Control.Monad (forever)

-- Assume these functions are defined in the corresponding cipher breaker modules.
-- For the purpose of a self-contained example, we provide dummy implementations.
caesarAttack :: String -> IO [AttackStat] -- FROM CaesarAndKeywordBreaker.lhs
caesarAttack ciphertext = return
  [ AttackStat "Caesar Attack" "dummy-caesar-key"
      (ciphertext) 0.12 90.0 85.0 ]

vigenereAttack :: String -> IO [AttackStat] -- FROM VigenereBreaker.lhs
vigenereAttack ciphertext = return
  [ AttackStat "Vigenere Attack" "dummy-vigenere-key"
      (ciphertext) 0.25 80.0 75.0 ]

playfairAttack :: String -> IO [AttackStat] -- FROM PlayfairBreaker.lhs
playfairAttack ciphertext = return
  [ AttackStat "Playfair Attack" "dummy-playfair-key"
      (ciphertext) 0.35 70.0 65.0 ]

--------------------------------------------------------------------------------
-- Types

-- | CipherType represents the type of cipher.
data CipherType = Caesar | Vigenere | Playfair
  deriving (Show, Eq)

-- | AttackStat holds the results from an attack.
data AttackStat = AttackStat {
    methodName    :: String,
    key           :: String,
    decipheredText :: String,
    runtime       :: Float,
    keyMatch      :: Float,
    textMatch     :: Float
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Cipher Recognition - FROM CipherRecognition.lhs

-- | Recognizes the cipher type from the input ciphertext. - currently a placeholder
cipherRecognizer :: String -> IO (Maybe CipherType)
cipherRecognizer input =
  let lowerInput = map toLower input
  in if "caesar" `isInfixOf` lowerInput
       then do
         putStrLn "\nCipher recognized! (Caesar)\n"
         return (Just Caesar)
     else if "vigenere" `isInfixOf` lowerInput
       then do
         putStrLn "\nCipher recognized! (Vigenere)\n"
         return (Just Vigenere)
     else if "playfair" `isInfixOf` lowerInput
       then do
         putStrLn "\nCipher recognized! (Playfair)\n"
         return (Just Playfair)
     else do
         putStrLn "\nCipher could not be recognized."
         return Nothing

--------------------------------------------------------------------------------
-- Attack Result Printer

-- | Pretty prints a list of AttackStat.
attackStatPrinter :: [AttackStat] -> IO ()
attackStatPrinter stats = mapM_ printStat stats
  where
    printStat stat = do
      putStrLn $ "Cipher used: " ++ methodName stat
      putStrLn $ "Key: " ++ key stat
      putStrLn $ "Deciphered text: " ++ decipheredText stat
      putStrLn $ "Runtime: " ++ show (runtime stat) ++ " seconds"
      putStrLn $ "Key match: " ++ show (keyMatch stat) ++ "%"
      putStrLn $ "Text match: " ++ show (textMatch stat) ++ "%"
      putStrLn ""

--------------------------------------------------------------------------------
-- Global Attack Function

runAttack :: (String -> IO [AttackStat]) -> String -> IO (Maybe [AttackStat])
runAttack attackFunc ciphertext = do
  stats <- attackFunc ciphertext
  attackStatPrinter stats
  return (Just stats)

-- | Executes the appropriate attack based on the recognized cipher.
attack :: Maybe CipherType -> String -> IO (Maybe [AttackStat])
attack Nothing _ = return Nothing
attack (Just cipherType) ciphertext =
  case cipherType of
    Caesar   -> runAttack caesarAttack ciphertext
    Vigenere  -> runAttack vigenereAttack ciphertext
    Playfair -> runAttack playfairAttack ciphertext


--------------------------------------------------------------------------------
-- Encrypt/Decrypt menu

encryptDecryptMenu :: IO ()
encryptDecryptMenu = do
    putStrLn "\n=== Encrypt / Decrypt Ciphers ==="
    putStrLn "Select the cipher: "
    putStrLn "1. Caesar"
    putStrLn "2. Vigenere"
    putStrLn "3. Playfair"
    putStr "Your choice: "
    hFlush stdout
    cipherChoice <- getLine

    putStrLn "\nEnter the key:"
    putStr "Key: "
    hFlush stdout
    key <- getLine

    putStrLn "\nEnter the message:"
    putStr "Message: "
    hFlush stdout
    message <- getLine

    case cipherChoice of
      "1" -> do
          let encoded = encryptCaesarAndKeyword key message
          let decoded = decryptCaesarAndKeyword key encoded
          putStrLn "\n--- Caesar Cipher ---"
          putStrLn $ "Encoded: " ++ encoded
          putStrLn $ "Decoded: " ++ decoded
      "2" -> do
          let encoded = vigenereEncode key message
          let decoded = vigenereDecode key encoded
          putStrLn "\n--- Vigenere Cipher ---"
          putStrLn $ "Encoded: " ++ encoded
          putStrLn $ "Decoded: " ++ decoded
      "3" -> do
          let encoded = encryptPlayfair key message
          let decoded = decryptPlayfair key encoded
          putStrLn "\n--- Playfair Cipher ---"
          putStrLn $ "Encoded: " ++ encoded
          putStrLn $ "Decoded: " ++ decoded
      _ -> putStrLn "Invalid cipher selection."

-- Recognize attack menu
recognizeAttackMenu :: IO ()
recognizeAttackMenu = do
  putStrLn "\n=== Recognize and Attack Cipher ==="
  putStr "Enter ciphertext: "
  hFlush stdout
  ciphertext <- getLine
  mCipher <- cipherRecognizer ciphertext
  _ <- attack mCipher ciphertext
  return ()

--------------------------------------------------------------------------------
-- Main Menu

main :: IO ()
main = forever $ do
  putStrLn $ concat (replicate 2 "\n---------------------------------------")
  putStrLn "\n=== Main Menu ==="
  putStrLn "1. Encrypt / Decrypt text"
  putStrLn "2. Recognize and attack cipher"
  putStr "Select an option (1 or 2, Ctrl+C to exit): "
  hFlush stdout
  option <- getLine
  case option of
    "1" -> encryptDecryptMenu
    "2" -> recognizeAttackMenu
    _   -> putStrLn "Invalid option selected."

\end{code}