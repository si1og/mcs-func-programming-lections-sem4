{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module MPTC where

import Control.Monad.Fail ( MonadFail(..) )
import Control.Monad.Reader
import Control.Monad.State

--------------------------------------------------------
---------- Мультипараметрические классы типов ----------
--------------------------------------------------------
{-

Классы типов имели один параметр:
class Class a where ... -- задает интерфейс для одного типа данных
            a - единственный параметр

instance Class Int where ... -- реализация интерфейса

Например: 
:i Functor
    class Functor f where ...
Представитель:
    instance Functor Maybe where ...



Мультипараметрические классы типов являются расширением:
{-# LANGUAGE MultiParamTypeClasses #-}



Пример умножение матриц (2x2):
-}

data Vector = Vector Int Int deriving (Eq, Show)
data Matrix = Matrix Vector Vector deriving (Eq, Show)

{-
Умножений матриц несколько

Реализуем матричное умножение для следующих типов:

(***) :: Matrix -> Matrix -> Matrix
(***) :: Matrix -> Vector -> Vector
(***) :: Matrix -> Int    -> Matrix
(***) :: Int    -> Matrix -> Matrix

(*)   :: a -> a -> a -- стандартное умножение

Для матриц нужно умножение типа:

(***) :: a -> b -> c



Соберем класс типов для матричного умножения:

class Mult a b c where -- реализация интерфейса для отношения между элементами типа (a b c)
                          т.е. для каждой тройки можно задать разное поведение
                          это не отдельные типы, они связаны
                          значительно более сложная структура по сравнению с однопараметрическим классом типов
           a b c -- неопределенность выходного типа -}
class Mult a b c where -- | a b -> c where -- {-# LANGUAGE FunctionalDependencies #-}
{-               | a b -> c -- класс типов c функционально зависит от параметров a b
                               т.е. если заданы параметры a b то c определяется однозначно
                               т.е. c по a b должно определяться однозначно-}
    (***) :: a -> b -> c

{-

В общем случае в 
class Mult a b c where
    (***) :: a -> b -> c
тип с - не зависит от a и b и может быть любым -- !sic

для обхода этой ситуации используется расширение:
{-# LANGUAGE FunctionalDependencies #-} -- задает зависимости типов, если a b заданы, то c определяется однозначно

Ограничение задается: ... | a b -> c ...

То есть написать одновременно: 
instance Mult Matrix Matrix Matrix where ...
и 
instance Mult Matrix Matrix Vector where ...
невозможно благодаря наложенным ограничениям и FunctionalDependencies.

-}

instance Mult Matrix Matrix Matrix where
    Matrix (Vector a11 a12) (Vector a21 a22) *** Matrix (Vector b11 b12) (Vector b21 b22) =
        Matrix (Vector c11 c12) (Vector c21 c22) where
            c11 = a11 * b11 + a12 * b21
            c12 = a11 * b12 + a12 * b22
            c21 = a21 * b11 + a22 * b21
            c22 = a21 * b12 + a22 * b22

-- let a = Matrix (Vector 1 2) (Vector 3 4)
-- let b = Matrix (Vector 1 0) (Vector 0 1)
-- a *** b
-- (a *** b) :: Matrix 

instance Mult Matrix Vector Vector where
    Matrix (Vector a11 a12) (Vector a21 a22) *** Vector b11 b12 =
        Vector c11 c12 where
            c11 = a11 * b11 + a12 * b12
            c12 = a21 * b11 + a22 * b12

-- let c = Vector 1 2
-- a *** c

instance Mult Matrix Int Matrix where
    Matrix (Vector a11 a12) (Vector a21 a22) *** b11 =
        Matrix (Vector c11 c12) (Vector c21 c22) where
            c11 = a11 * b11
            c12 = a12 * b11
            c21 = a21 * b11
            c22 = a22 * b11
            
-- a *** (9 :: Int)

instance Mult Int Matrix Matrix where
    b11 *** Matrix (Vector a11 a12) (Vector a21 a22) =
        Matrix (Vector c11 c12) (Vector c21 c22) where
            c11 = a11 * b11
            c12 = a12 * b11
            c21 = a21 * b11
            c22 = a22 * b11

-- (9 :: Int) *** a



--------------------------------------------------------
{-
К текущему моменту использовались монады и привязанные к ним интерфейсы

 интерфейс
 🠗
ask :: Reader r r
       --------
           🠕
           конкретная монада


Хотим наделять произвольную монаду произвольным интерфейсом
    Для этого нужно обобщить конкретную монаду до произвольной:

                   m - произвольная монада  🠔 🠔 🠔 🠔 🠔 🠔
                   🠗                                      🠕
               --------                                   🠕
        ask :: Reader r r                                 🠕
                        🠕                                 🠕
                        должен извлекаться из конкретного m
                                (зависимость r от m)



mtl - расширенный transformers
Стандартные интерфейсы стандартных монад упакованы в mtl в мультипараметрические классы типов,
это позволяет наделять любую монаду соответствующим интерфейсом


(MonadReader r m) - расширяет класс типов (Monad m) дополнительным параметром r, который задает окружение -- (MonadReader r m)
                    и это окружение восстанавливается однозначно по m -- (| m -> r )
class Monad m => MonadReader r m | m -> r where -- мультипараметрический класс типов
                                                -- ask :: Reader r r
                                                -- ask :: (->)   r r
                                                --       '----.---'
                                                -- ask ::     m    r -- можно абстрагироваться по Reader
    ask :: m r                                  -- может быть расширен для любой монады
                                                -- т.е. любая монада определившая ask и local становится Reader
    local :: (r -> r) -> m a -> m a

instance MonadReader r (Reader r) where -- представитель MonadReader для монады Reader
    ask = aks
    local = local

class (Monoid w, Monad m) => MonadWriter w m | m -> w where
    tell :: w -> m ()
    listen :: m a -> m (a, w)
class Monad m => MonadState s m | m -> s where
    get :: m s
    put :: s -> m ()
class Monad m => MonadError e m | m -> e where
    throwError :: e -> m a
    catchError :: m a -> (e -> m a) -> m a


-- Пример:
Неупакованная в Reader частично примененная стрелка (->) объявлена представителем MonadReader

class Monad m => MonadReader r m | m -> r where
    ask :: m r
    local :: (r -> r) -> m a -> m a

instance MonadReader r ((->) r) where
    ask = id -- связывает внутреннюю r со внешней r
    local f m = m . f
-- do {x <- (*2); y <- ask; return (x+y)} $ 5
--          (*2) -- прямое использование частично примененной функциональной стрелки (фактически это неупакованный Reader)
--                     ask -- используем интерфейс MonadReader r m | m -> r
--                         -- в данном случае просто id


Тип Either объявлен представителем MonadError

class Monad m => MonadError e m | m -> e where
    throwError :: e -> m a
    catchError :: m a -> (e -> m a) -> m a
instance MonadError e (Either e) where
    throwError = Left
    Left l `catchError` h = h l
    Right r `catchError` _ = Right r

Можно использовать throwError и catchError без упаковки Except:
-- Left 5 `catchError` (\e -> Right (e^2))
-- Right 5 `catchError` (\e -> Right (e^2))


-- Мультипараметрические классы типов реализуют 
возможность использовать вложенные интерфейсы композитных монад и предоставлять внутренние интерфейсы наружу
-}

incrementCounter :: MonadState Int m => m () -- FlexibleContexts
--                              State Int ()
incrementCounter = do
    count <- get
    put (count + 1)
-- do { result <- runStateT incrementCounter 0; print result; return () }

readerStatePr :: ReaderT String (StateT Int IO) ()
--                       String - окружение
--                                      Int - состояние
readerStatePr = do
    env <- ask -- чтение окружения (Reader)
    modify (+ 1) -- изменение состояния (State)
    liftIO $ putStrLn $ "Env: " ++ env
-- do {result <- runStateT (runReaderT readerStatePr "test") 0; print result; return ()}
