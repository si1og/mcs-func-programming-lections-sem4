module MonadState where

import Control.Monad.State ( State, state, execState, runState, evalState, get, gets, put, modify )
import Control.Monad ( replicateM )

--------------------------------------------------------
{-
Монада State
Эффект мутеабельного состояния (объединение монады Reader и Writer)
  избавляет от необходимости протаскивать состояние вручную


newtype State s a = State { runState :: s -> (a,s) } -- композиция частично примененной стрелки и пары
state :: (s -> (a,s)) -> State s a -- упаковка состояния
runState :: State s a -> s -> (a,s) -- распаковка стрелки

instance Monad (State s) where
    return x = state $ \st -> (x,st) -- упаковка без изменения состояния
    m >>= k = state $ -- стандартная упаковка
        \st -> let (x,st') = runState m st -- извлечение стрелки для новой пары и создание нового состояния
                   m' = k x -- значне передается в стрелку Клейсли
               in runState m' st' -- упаковываем новое состояние

-- Запуск вычислений:
runState :: State s a -> s -> (a,s)
execState :: State s a -> s -> s -- получение только состояния
evalState :: State s a -> s -> a -- получение только значения

runState (return 3 :: State String Int) "Hi, State!"
execState (return 3 :: State String Int) "Hi, State!"
evalState (return 3 :: State String Int) "Hi, State!"

-- Стандартный интерфейс

get :: State s s                -- ask
get = state $ \s -> (s,s) -- передает состояние в значение

put :: s -> State s ()          -- tell
put s = state $ \_ -> ((),s) -- игнорирует существующее состояние и записывает пользовательские данные без значения

modify :: (s -> s) -> State s ()-- принимает функцию из состояния в состояние
modify f = do s <- get
              put (f s)

gets :: (s -> a) -> State s a   -- в значение записывает состояние обработанное функцией
gets f = do s <- get
            return (f s)

-}

tick :: State Int Int
tick = do 
  n <- get
  put (n + 1)
  return n
-- runState tick 3

--runState (state (\s -> (s,s)) >>= (\n -> state $ \_ -> (n,n+1))                    ) 3
--runState (state (\s -> (s,s)) >>= \n -> (state $ \_ -> ((),n+1)) >>= \_ -> return n) 3
--runState (get                 >>= \n -> put (n + 1)              >>= \_ -> return n) 3

succ' :: Int -> Int
succ' n = execState tick n
-- succ' 3

plus :: Int -> Int -> Int
plus n x = execState (sequence $ replicate n tick) x
-- plus 4 3

-- replicateM :: Applicative m => Int -> m a -> m [a]
-- replicateM n = sequenceA . replicate n

plus' :: Int -> Int -> Int
plus' n x = execState (replicateM n tick) x
-- plus' 4 3

