\subsection{Playfair cipher}\label{sec:playfair}

In our Haskell implementation of the Playfair cipher, we express the algorithm in a functional style, highlighting Haskells strengths in handling complex string manipulations and modular code design.

The core components of our implementation include:

\begin{itemize}
  \item \textbf{Table Construction:}  
        We generate a 5 x 5 table from a user-supplied keyword by removing duplicate letters and appending the remaining letters of the alphabet (with 'J' treated as 'I'). This process is performed by the \texttt{createTable} function.
  \item \textbf{Text Preparation:}  
        The plaintext is filtered to remove non-letter characters, converted to uppercase, and normalized (replacing 'J' with 'I') using the \texttt{prepareText} function.
  \item \textbf{Digram Generation:}  
        The normalized text is split into pairs (digrams) with the \texttt{makeDigrams} function. When duplicate letters occur within a pair, a filler (typically 'X') is inserted to ensure proper encryption.
  \item \textbf{Encryption and Decryption:}  
        Depending on whether a pair of letters is in the same row, same column, or forms the corners of a rectangle in the table, we perform the appropriate substitution. This logic is implemented in the \texttt{encryptDigram} and \texttt{decryptDigram} functions, while the overall encryption and decryption are handled by the \texttt{encryptPlayfair} and \texttt{decryptPlayfair} functions.
\end{itemize}

We begin with the module declaration:

\begin{code}
module Playfair where
\end{code}

Below are the key functions used in our implementation:

\begin{code}
import Data.Char (toUpper, isAlpha)
import Data.List (nub)

-- The alphabet for the cipher (note that 'J' is omitted)
alphabet :: [Char]
alphabet = filter (/= 'J') ['A'..'Z']
\end{code}

\textbf{Table Construction:}  
The \texttt{createTable} function builds the 5 x 5 table by first cleaning the keyword and then appending any missing letters.
\begin{code}
createTable :: String -> [Char]
createTable keyword =
  let cleaned   = map (\c -> if c == 'J' then 'I' else c)
                  $ filter isAlpha $ map toUpper keyword
      keyUnique = nub cleaned
  in keyUnique ++ filter (`notElem` keyUnique) alphabet
\end{code}

\textbf{Text Preparation:}  
The \texttt{prepareText} function filters and normalizes the input text.
\begin{code}
prepareText :: String -> String
prepareText = map (\c -> if c == 'J' then 'I' else c)
            . filter isAlpha . map toUpper
\end{code}

\textbf{Digram Generation:}  
The \texttt{makeDigrams} function splits the text into pairs (digrams), inserting an 'X' when two consecutive letters are identical or when padding is required.
\begin{code}
makeDigrams :: String -> [String]
makeDigrams []       = []
makeDigrams [x]      = [[x, 'X']]
makeDigrams (x:y:xs)
  | x == y    = [x, 'X'] : makeDigrams (y:xs)
  | otherwise = [x, y]   : makeDigrams xs
\end{code}

\textbf{Finding Character Positions:}  
The helper function \texttt{chunksOf} is used to break lists into chunks and \texttt{findPosition} locates a character within the 5×5 table.
\begin{code}
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

findPosition :: [[Char]] -> Char -> (Int, Int)
findPosition table c =
  let positions = [ (r, col)
                  | (r, row) <- zip [0..] table
                  , (col, ch) <- zip [0..] row
                  , ch == c ]
  in case positions of
       []    -> error ("Character not found: " ++ [c])
       (p:_) -> p
\end{code}

\textbf{Encryption and Decryption:}  
The functions \texttt{encryptDigram} and \texttt{decryptDigram} perform the letter substitutions based on the rules of the Playfair cipher.
\begin{code}
encryptDigram :: [[Char]] -> String -> String
encryptDigram table [a,b] =
  let (rowA, colA) = findPosition table a
      (rowB, colB) = findPosition table b
  in if rowA == rowB then
       -- Same row: shift right
       [ table !! rowA !! ((colA + 1) `mod` 5)
       , table !! rowB !! ((colB + 1) `mod` 5) ]
     else if colA == colB then
       -- Same column: shift down
       [ table !! ((rowA + 1) `mod` 5) !! colA
       , table !! ((rowB + 1) `mod` 5) !! colB ]
     else
       -- Rectangle: swap columns
       [ table !! rowA !! colB
       , table !! rowB !! colA ]
encryptDigram _ _ = error "Invalid digram length"

decryptDigram :: [[Char]] -> String -> String
decryptDigram table [a,b] =
  let (rowA, colA) = findPosition table a
      (rowB, colB) = findPosition table b
  in if rowA == rowB then
       -- Same row: shift left
       [ table !! rowA !! ((colA - 1) `mod` 5)
       , table !! rowB !! ((colB - 1) `mod` 5) ]
     else if colA == colB then
       -- Same column: shift up
       [ table !! ((rowA - 1) `mod` 5) !! colA
       , table !! ((rowB - 1) `mod` 5) !! colB ]
     else
       -- Rectangle: swap columns
       [ table !! rowA !! colB
       , table !! rowB !! colA ]
decryptDigram _ _ = error "Invalid digram length"
\end{code}

Finally, the overall \texttt{encryptPlayfair} and \texttt{decryptPlayfair} functions tie these components together:
\begin{code}
encryptPlayfair :: String -> String -> String
encryptPlayfair keyword text =
  let tableFlat = createTable keyword
      table     = chunksOf 5 tableFlat
      prepared  = prepareText text
      digrams   = makeDigrams prepared
  in concatMap (encryptDigram table) digrams

decryptPlayfair :: String -> String -> String
decryptPlayfair keyword text =
  let tableFlat = createTable keyword
      table     = chunksOf 5 tableFlat
      digrams   = chunksOf 2 (prepareText text)
  in concatMap (decryptDigram table) digrams
\end{code}

In summary, our Playfair cipher implementation in Haskell clearly demonstrates how breaking down a problem into small, composable functions can result in clean, modular, and maintainable code. By leveraging Haskell’s strong type system and functional abstractions, we can model classical encryption algorithms in an elegant and concise manner.
