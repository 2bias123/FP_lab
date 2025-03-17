\subsection{Playfair cipher}\label{sec:playfair}

Explain code of cipher implementation in codeblocks, unhide important things from below

\begin{code}
module Playfair where
\end{code}

\hide{
\begin{code}

import Data.Char (toUpper, isAlpha)
import Data.List (nub)

-- | Convert the alphabet to uppercase and filter out 'J'.
alphabet :: [Char]
alphabet = filter (/= 'J') ['A'..'Z']

-- | Given a keyword, build a table (as a flat list of 25 characters) for the Playfair cipher.
createTable :: String -> [Char]
createTable keyword =
  let cleaned = map (\c -> if c == 'J' then 'I' else c) $
                filter isAlpha $ map toUpper keyword
      keyUnique = nub cleaned
  in keyUnique ++ filter (`notElem` keyUnique) alphabet

-- | Break a list into chunks of n elements.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Prepare the text by converting to uppercase, removing non-letters, and replacing 'J' with 'I'.
prepareText :: String -> String
prepareText = map (\c -> if c == 'J' then 'I' else c) . filter isAlpha . map toUpper

-- | Split the text into digrams, inserting an 'X' between duplicate letters in the same pair,
--   and padding with an 'X' if the final digram is incomplete.
makeDigrams :: String -> [String]
makeDigrams []       = []
makeDigrams [x]      = [[x, 'X']]
makeDigrams (x:y:xs)
  | x == y    = [x, 'X'] : makeDigrams (y:xs)
  | otherwise = [x, y]   : makeDigrams xs

-- | Find the (row, column) of a character in the 5x5 table.
findPosition :: [[Char]] -> Char -> (Int, Int)
findPosition table c =
  let positions = [ (r, col) 
                  | (r, row) <- zip [0..] table
                  , (col, ch) <- zip [0..] row
                  , ch == c ]
  in case positions of
       []    -> error ("Character not found: " ++ [c])
       (p:_) -> p

-- | Encrypt a digram using the Playfair rules.
encryptDigram :: [[Char]] -> String -> String
encryptDigram table [a,b] =
  let (rowA, colA) = findPosition table a
      (rowB, colB) = findPosition table b
  in if rowA == rowB then
       -- Same row: shift right (wrap around)
       [ table !! rowA !! ((colA + 1) `mod` 5)
       , table !! rowB !! ((colB + 1) `mod` 5) ]
     else if colA == colB then
       -- Same column: shift down (wrap around)
       [ table !! ((rowA + 1) `mod` 5) !! colA
       , table !! ((rowB + 1) `mod` 5) !! colB ]
     else
       -- Rectangle: swap columns.
       [ table !! rowA !! colB
       , table !! rowB !! colA ]
encryptDigram _ _ = error "Invalid digram length"

-- | Decrypt a digram using the reverse Playfair rules.
decryptDigram :: [[Char]] -> String -> String
decryptDigram table [a,b] =
  let (rowA, colA) = findPosition table a
      (rowB, colB) = findPosition table b
  in if rowA == rowB then
       -- Same row: shift left (wrap around)
       [ table !! rowA !! ((colA - 1) `mod` 5)
       , table !! rowB !! ((colB - 1) `mod` 5) ]
     else if colA == colB then
       -- Same column: shift up (wrap around)
       [ table !! ((rowA - 1) `mod` 5) !! colA
       , table !! ((rowB - 1) `mod` 5) !! colB ]
     else
       -- Rectangle: swap columns.
       [ table !! rowA !! colB
       , table !! rowB !! colA ]
decryptDigram _ _ = error "Invalid digram length"

-- | Encrypt the entire message.
encrypt :: String -> String -> String
encrypt keyword text =
  let tableFlat = createTable keyword
      table     = chunksOf 5 tableFlat
      prepared  = prepareText text
      digrams   = makeDigrams prepared
  in concatMap (encryptDigram table) digrams

-- | Decrypt the entire ciphertext.
decrypt :: String -> String -> String
decrypt keyword text =
  let tableFlat = createTable keyword
      table     = chunksOf 5 tableFlat
      digrams   = chunksOf 2 (prepareText text)
  in concatMap (decryptDigram table) digrams

-- | Main: interact with the user.
main :: IO ()
main = do
  putStrLn "Enter a keyword (letters only, J will be treated as I):"
  keyword <- getLine
  putStrLn "Enter the message(example: together we can reach the end):"
  message <- getLine
  let cipherText = encrypt keyword message
  putStrLn $ "\nEncrypted message:\n" ++ cipherText
  let plainText  = decrypt keyword cipherText
  putStrLn $ "\nDecrypted message:\n" ++ plainText

\end{code}
}