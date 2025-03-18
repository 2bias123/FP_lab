\section{Tests}
\label{sec:tests}

We now use the library QuickCheck to randomly generate input for the ciphers, and test whether they work correctly.
We can also use QuickCheck to test attacks maybe?

\begin{code}
module Tests where

import Vignere
import Substitution
import Playfair

import Test.Hspec
import Test.QuickCheck

-- Define the functions that are referenced in the tests
somenumbers :: [Int]
somenumbers = [1..10]

funnyfunction :: Int -> Int
funnyfunction n = n * 2

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = myreverse xs ++ [x]

main :: IO ()
main = hspec $ do
  describe "Basics" $ do
    it "somenumbers should be the same as [1..10]" $
      somenumbers `shouldBe` [1..10]
    it "if n > - then funnyfunction n > 0" $
      property (\n -> n > 0 ==> funnyfunction n > 0)
    it "myreverse: using it twice gives back the same list" $
      property $ \str -> myreverse (myreverse str) == (str::String)
\end{code}
