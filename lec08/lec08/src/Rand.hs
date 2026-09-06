module Rand ( rand ) where

{-
Реализуем функцию, которая генерирует случайное число
-}

import System.Random (randomRIO)

minDie :: Int
minDie = 1

maxDie :: Int
maxDie = 6

{-
randomRIO :: (Random a, MonadIO m) => (a, a) -> m a
    нарушает правило ссылочной прозрачности, т.к. возвращает разные значения

print :: Show a => a -> IO ()
-}

rand :: IO ()
rand = do
    dieRoll <- randomRIO (minDie, maxDie)
    print dieRoll
