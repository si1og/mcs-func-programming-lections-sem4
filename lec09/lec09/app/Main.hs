-------------------------------------------------------------------------
-- Лекция 09 Монады Id, Maybe, Either, List, Reader, Writer, State, IO --
-------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main (main) where

import Control.Applicative (Alternative (empty, (<|>)), asum, (<**>))
import Control.Monad (MonadPlus (..), guard, mfilter, msum)
import Control.Monad.Except (Except, MonadError (catchError, throwError), runExcept)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Trans.Reader (Reader, ask, asks, local, reader, runReader)
import Control.Monad.Trans.Writer (Writer, censor, execWriter, listen, listens, runWriter, tell, writer)
import MonadExcept
import MonadId
import MonadList
import MonadMaybe
import MonadReader
import MonadWriter

main :: IO ()
main = putStrLn "Monads: Id, Maybe, Either, List, Reader, Writer"

--------------------------------------------------------
{-
Использование чистых функций с контейнерами и контекстами:
($)  ::                    (a -> b) ->    a ->    b
map  ::                    (a -> b) -> [] a -> [] b
<$>  :: Functor f     =>   (a -> b) -> f  a -> f  b
<*>  :: Applicative f => f (a -> b) -> f  a -> f  b
pure :: Applicative f =>                  a -> f  a

Связывание чистых значений с эффектами
Расширение чистых функций (a -> b) до вычислений с эффектами:
    - отсутствие эффектов:             a -> Identity b
    - завершение неудачей:             a -> Maybe b
    - множественные результаты:        a -> [b]
    - завершение с ошибкой:            a -> (Either s) b
    - запись в лог:                    a -> (s,b)
    - чтение из внешнего окружения:    a -> ((->) e) b
    - работа с мутабельным состоянием: a -> (State s) b
    - ввод/вывод (файлы, консоль):     a -> IO b

Обобщением эффектов является стрелка Клейсли: a -> m b
(стрелка Клейсли обеспечивает зависимость эффекта от значения,
т.е. можно управлять эффектами с помощью значений)

a -> m b
     m :: * -> * - однопараметрический конструктор типов (контекст)
                на место m подставляются конструкторы, которые обеспечивают эффекты

       это значение, которое управляет эффектом
       🠗                     -}
amb = \n -> replicate n 'A' {- эффект множества результатов
                                        🠕
                                        здесь эффект, который управляется значением

                            на уровне типов (amb :: Int -> [Char]) функция имеет одно значение на входе и множество на выходе

                            Требования к оператору над типами m в стрелке Клейсли:
                            1. Универсальный интерфейс для упаковки значения в контейнере
                                a -> m b
                            2. Универсальный интерфейс для композиции вычислений с эффектами (стрелок Клейсли)
                                (<=<) :: (b -> m c) -> (a -> m b) -> (a -> m c)             - fish operator

                                Через интерфейс функтора оператор (<=<) не реализовать:
                                   k1 :: (b -> m c)
                                                 k2 :: (a -> m b)
                                                                     (a -> m c)
                                   k1    <=<     k2               =  \x -> k1 <$> k2 x
                                                                                  k2 x :: m b
                                                                           k1 <$> k2 x -- применяет k1 к контейнеру (m b) через fmap
                                                                                       -- результат m (m b)
                                <$> :: (a -> b) - > m a -> m b
                                       (a -> m b) -> m a -> m (m b)
                                нужен: join :: m (m a) -> m a
                            3. ! НЕТ универсального интерфейса для извлечения значения из контейнера m (эффект в общем случае не обратим)

                            Класс типов Monad
                            type Monad :: (* -> *) -> Constraint
                            class Applicative m => Monad m where
                                (>>=) :: m a -> (a -> m b) -> m b
                                (>>) :: m a -> m b -> m b
                                return :: a -> m a
                                {-# MINIMAL (>>=) #-}

                            Облегченный bind - берет эффекты из двух контекстов
                            (>>) :: m a -> m b -> m b
                                    m1 >>  m2 ≡
                                  ≡ m1 >>= \_ -> m2

                            (*>) :: f a -> f b -> f b -- аналогия с Applicative

                            Just (+) >> Just 3 >> Just 4
                            Just (+) >> Nothing >> Just 4
                            Just (+) *> Just 3 *> Just 4
                            Just (+) >>= Just 3 >>= Just 4 -- ?

                            return :: a -> m a
                            return определяет тривиальную стрелку Клейсли
                                   поднимает чистое вычисление в контекст не добавляя эффектов
                            return = pure -- реализация по умолчанию берется из Applicative

                            Используем return для реализации стрелки Клейсли (f :: a -> b):
                            -}

toKleisli :: (Monad m) => (a -> b) -> (a -> m b)
toKleisli f = return . f

{-
:t cos
:t toKleisli cos
(toKleisli cos 0) :: Maybe Double
(toKleisli cos 0) :: [Double]
(toKleisli cos 0) :: IO Double

Just 5 >>= Just . (+2) >>= Just . (*3) >>= Just . (+1)
:t Just . (+2)
:t Just

($)    ::                     (a ->   b) ->   a ->   b
(<$>)  :: Functor     f =>    (a ->   b) -> f a -> f b
(<*>)  :: Applicative f =>  f (a ->   b) -> f a -> f b
(=<<)  :: Monad       m =>    (a -> m b) -> m a -> m b

(&)    ::                     a ->   (a ->   b) ->   b
(<&>)  :: Functor     f =>  f a ->   (a ->   b) -> f b
(<**>) :: Applicative f =>  f a -> f (a ->   b) -> f b
(>>=)  :: Monad       m =>  m a ->   (a -> m b) -> m b

Эффекты у аппликатива идут в разном порядке у обычной и flip версии
-}
f1 :: IO (Int -> String)
f1 = putStrLn "Вывод функции" >> return show

f2 :: IO Int
f2 = putStrLn "Вывод аргумента" >> return 42

{-
Эффекты у аппликатива идут в разном порядке у обычной и flip версии:
f1 <*> f2
f2 <**> f1

         f1                <*> f2
(<*>) :: f  (a   -> b)      -> f  a   -> f  b
   f1 :: IO (Int -> String)
                         f2 :: IO Int
                                         IO String

          f2 <**> f1
(<**>) :: f  a -> f  (a   -> b)      -> f  b
    f2 :: IO Int
            f1 :: IO (Int -> String)
                                        IO String

Эффекты у монад идут в том же порядке:
putStrLn "start" >>= \_ -> putStrLn "end"
(\_ -> putStrLn "end") =<< putStrLn "start"

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
-}
