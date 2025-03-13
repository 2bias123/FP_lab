module Main where

import Data.Char

vigenereEncode :: String -> String -> String
vigenereEncode key text = enc (cycle key) text
  where
    enc _ [] = []
    enc (k:ks) (t:ts)
      | isAlpha t = shift k t : enc ks ts
      | otherwise = t : enc ks ts
    shift k t =
      let base = if isUpper t then ord 'A' else ord 'a'
          offset = ord (toUpper k) - ord 'A'
      in chr $ base + mod (ord t - base + offset) 26

vigenereDecode :: String -> String -> String
vigenereDecode key text = enc (cycle key) text
  where
    enc _ [] = []
    enc (k:ks) (t:ts)
      | isAlpha t = unshift k t : enc ks ts
      | otherwise = t : enc ks ts
    unshift k t =
      let base = if isUpper t then ord 'A' else ord 'a'
          offset = ord (toUpper k) - ord 'A'
      in chr $ base + mod (ord t - base - offset) 26


io :: IO ()
io = do
  let key = "KEY"
  putStrLn "\nEnter the message to encode:"
  message <- getLine
  let encoded = vigenereEncode key message
  let decoded = vigenereDecode key encoded
  putStrLn $ "Encoded : " ++ encoded
  putStrLn $ "Decoded : " ++ decoded

main :: IO ()
main = do
  let key = "KEY"
  let message = "HELLO WORLD"
  let encoded = vigenereEncode key message
  let decoded = vigenereDecode key encoded
  putStrLn $ "Original: " ++ message
  putStrLn $ "Encoded : " ++ encoded
  putStrLn $ "Decoded : " ++ decoded