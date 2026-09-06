--------------------------------------------------------
-- Лекция 10 State, IO, RWS, logic, parallel, deepseq --
--------------------------------------------------------

{-
--------------------------------------------------------
Монады из стандартной библиотеки: 
    - MonadId     - отсутствие эффектов:             a -> Identity b
    - MonadMaybe  - завершение неудачей:             a -> Maybe b
    - MonadExcept - завершение с ошибкой:            a -> (Either s) b
    - MonadList   - множественные результаты:        a -> [b]
    - MonadWriter - запись в лог:                    a -> (s,b)
    - MonadReader - чтение из внешнего окружения:    a -> ((->) e) b
    - MonadState  - работа с мутабельным состоянием: a -> (State s) b
    - MonadIO     - ввод/вывод (файлы, консоль):     a -> IO b

Дополнительные монады и библиотеки:
    - MonadRWS    - внешнее окружение, лог и работы с мутабельными состояниями в одной монаде
    - MonadLogic  - вычислений с возвратом
    - MonadPar    - параллельные и конкурентные вычисления вычислений

-}

module Main where

import Control.Monad.Logic
import Control.Monad ( guard, MonadPlus(..), msum, mplus )
import qualified Control.Monad.RWS as RWS
import Control.Parallel.Strategies

import MonadState
import MonadIO
import MonadRWS
import MonadLogic
import MonadPar

main :: IO ()
main = 
    putStrLn "Monads: State, IO, Logic, Par, RWS, STRef\n" >>
    putStrLn "2 sparks Fib calculation:" >>
    mainPar >>
    
    --putStrLn "\nSlow & Fast factors:" >>
    --mainParTimeMesure >>

    --putStrLn "\nThread. Chan:" >>
    --mainChan >>

    --putStrLn "\nThread. Chan. Fix:" >>
    --mainChanFix >>

    --putStrLn "\nSTM:" >>
    --mainSTM >>
    
    putStrLn "\nDone"

--stack exec --rts-options="-N4 -s" lec10-exe
--stack exec -- lec10-exe +RTS -N -s

