
\subsection{Attacks based on recognition}\label{sec:allattacks}
This section should be the main code part, where attacks are done based on recognized cipher.

\begin{code}
module Main where

import Vigenere
import Substitution
import Playfair
import PlayfairBreaker

main :: IO ()
main = do
  putStrLn "Hello!"
\end{code}

We can show the output with

\begin{verbatim}
Hello!
Output, statistics, timings, encoded decoded results whatever:
[1,2,3,4,5,6,7,8,9,10]
[100,100,300,300,500,500,700,700,900,900]
[1,3,0,1,1,2,8,0,6,4]
[100,300,42,100,100,100,700,42,500,300]
GoodBye
\end{verbatim}
