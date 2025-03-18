\subsection{Attack on Substitution Cipher}\label{sec:substitutionattack}

\hide{
\begin{code}


bruteforceSubstitution :: Int -> String -> [(Int, String)]
bruteforceSubstitution iteration ciphertext
    |iteration > 26 = []
    |otherwise =
        let plaintext = decryptSubstitution iteration ciphertext
        in (iteration, plaintext) : bruteforceSubstitution (iteration + 1) ciphertext

bruteforceCaesar :: String -> [(Int, String)]
bruteforceCaesar = bruteforceSubstitution 1

\end{code}
}