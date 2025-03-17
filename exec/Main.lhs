\section{Attacks}\label{sec:Main}

In this section we perform attacks on the ciphers implemented in \ref{sec:implementation}.

\begin{code}
module Main where

import Vignere
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
