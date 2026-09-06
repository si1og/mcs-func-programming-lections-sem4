module Main (main) where

import MPTC
import TrMon
import TrRW

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity

main :: IO ()
main = putStrLn "Multi-parameter type class. Monad transformers"
