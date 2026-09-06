--------------------------------------
-- Лекция 12 Cont. Rand. QuickCheck --
--------------------------------------

module Main (main) where

import Control.Monad (when)
import System.Random (mkStdGen)
import System.Random.Stateful
import Data.List (sort)
import Data.Char (isAscii, isControl, isLetter)
import Test.QuickCheck

import MyCont
import MyRand
import MyQC


main :: IO ()
main = putStrLn "Cont. Rand. QuickCheck"
