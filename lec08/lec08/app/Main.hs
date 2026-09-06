module Main (main) where

import Rand (rand)
import Hello (hello)
import Pizza (pizza)
import Palindrome ( palindrome )
import Searcher ( )

main :: IO ()
main = do
    rand
    hello
    pizza
    palindrome
