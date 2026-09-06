{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module TrMon where

import Control.Monad.Writer
import Control.Monad.Reader
-- import Control.Monad.Trans.State -- transformers
import Control.Monad.State -- mtl
import Control.Monad.Identity ( Identity(..) )
import Control.Monad (msum, mfilter, guard, MonadPlus(..), ap, liftM)
import Control.Applicative (Alternative(empty, (<|>)))
import Data.Monoid (First(..), Sum(..), Product(..))
import Data.Maybe (fromMaybe)

--------------------------------------------------------
------------------ Monad transformers ------------------
---------------- Преобразователи монад -----------------
--------------------------------------------------------
{-
В лекции 9 решалась задача логирования запросов покупателя в Овощной лавке
    В решении присутствовала проблема намертво зашитого списка цен

Из набора стандартных монад Reader дает доступ к внешнему окружению,
    что решает недостатки реализации

Задача: объединить две монады
(В данном случае Reader и Writer)

Прошлая версия реализации:
-}

type Vegetable = String
type Price = Double
type Qty = Double
type Cost = Double
type PriceList = [(Vegetable,Price)]

prices :: PriceList
prices = [("Potato",13),("Tomato",55),("Apple",48)]

addVegetable' :: Vegetable -> Qty -> Writer (Sum Cost) (Vegetable, Price)
addVegetable' veg qty = do
    let pr = fromMaybe 0 $ lookup veg prices
                                    --  🠕
                                    -- prices зашито намертво, хотелось бы передавать внешнее окружение
    let cost = qty * pr
    tell $ Sum cost
    return (veg, pr)
-- runWriter $ addVegetable' "Apple" 100

{-
В общем случае композиция монад не является монадой, т.к. монада слишком глубоко влияет на структуру контейнера
    Написать универсальную монаду, которая является композицией двух произвольных монад невозможно

НО можно обернуть абстрактную монаду в конкретную монаду
    т.е вид: (конкретная монада (абстрактная монада))

Композиция монад не коммутирует, писать код в обе стороны



Трансформер монад — конструктор типа, который принимает монаду в качестве аргумента и возвращает монаду как результат
                                              ----------------                        -----------------

Кайнд монады m : * -> * -- однопараметрический конструктор

Кайнд трансформера
t m : * -> *
t : (* -> *) -> * -> * -- принимает монаду

Цепочки трансформеров: t1 (t2 (t3 (...)))

Терминирование:
Identity - для чистых вычислений
IO - для ввода/вывода
t1 (t2 (t3 Identity)) :: * -> *
t1 (t2 (t3 Identity)) a :: *
           IO
-- стандартные способы терминирования монад - Id или IO
-}

addVegetable :: Vegetable -> Qty 
               -> WriterT (Sum Cost)                    -- t1 (
                          (ReaderT PriceList Identity)  --     t2 (Id))
                          (Vegetable, Price)            --              a
{-              ...
               -> WriterT (Sum Cost)         -- трансформер
                          (Reader PriceList) -- внутренняя монада (терминирована внутри библиотеки)
                          (Vegetable, Price) -- тип, возвращаемый композитной монадой
-}
addVegetable veg qty = do
    priceList <- lift ask -- lift - протаскивает (на один уровень) ask через монаду WriterT 
    let pr = fromMaybe 0 $ lookup veg priceList
    let cost = qty * pr
    tell $ Sum cost
    return (veg, pr)
{-
runIdentity $ runReaderT (runWriterT $ addVegetable "Apple" 100) prices
                                       addVegetable "Apple" 100         -- передает два параметра (Vegetable -> Qty) в функцию addVegetable и возвращает монаду WriterT
                          runWriterT                                    -- снимает обертку WriterT и возвращает внутреннюю монаду ReaderT
              runReaderT                                         prices -- снимает обертку ReaderT, передает окружение prices и возвращает внутреннюю монаду Identity
runIdentity                                                             -- снимает обертку Identity и возвращает значение
runIdentity $ runReaderT (runWriterT $ addVegetable "Apple" 100) prices



Для запуска монады можно написать функции переходники, облегчающие читаемость кода
Организация запуска монады:
-}
runMonads :: Vegetable -> Qty -> PriceList -> ((Vegetable, Price), Sum Cost)
runMonads veg qty pr = runIdentity $ runReaderT (runWriterT $ addVegetable veg qty) pr
-- runMonads "Apple" 100 prices



{-
Требования к трансформерам монад:

1 Тип данных трансформера должен иметь кайнд: (* -> *) -> * -> *

2 Для любой монады m, аппликация t m должна быть монадой,
    то есть её return и >>= должны удовлетворять законам монад

3 Нужен lift :: m a -> t m a, «поднимающий» значение из трансформируемой монады в трансформированную.

В библиотеке transformers функция lift всегда вызывается вручную, в mtl — только для неоднозначных ситуаций
-}

stInteger :: State Integer Integer       
stInteger = do modify (+1)
               get
-- evalState stInteger 0
         
stString :: State String String       
stString = do modify (++"1")
              get
-- evalState stString "0"

stComb :: StateT Integer (StateT String Identity) (Integer, String)      
stComb = do modify (+1)
            lift $ modify (++"1")
            a <- get
            b <- lift get -- lift обязателен
            return (a,b)
-- runIdentity $ evalStateT (evalStateT stComb 0) "0"





--------------------------------------------------------
{-
Принцип создания собственного трансформера монад:

Исходные данные:
MyMonad - монада и остальные представители классов типов реализована за кадром

Нужно написать:
MyMonadT - трансформер для MyMonad



Для этого нужно выполнить три шага:

1. Тип данных трансформера должен иметь кайнд (* -> *) -> * -> *

newtype MyMonadT m a                            -- упаковка монады (m - та монада которую оборачиваем нашей монадой)
    = MyMonadT { runMyMonadT :: m (MyMonad a) } -- конструктор данных (MyMonadT) и распаковка монады для доступа к внутренней монаде

computation :: MyMonadT Identity a -- вычисление

runIdentity $ runMyMonadT computation :: MyMonad a -- запуск вычисления
              runMyMonadT computation -- снимает MyMonad и оставляет монаду Identity


2. Для любой монады m, аппликация t m должна быть монадой.

instance Monad m => Monad (MyMonadT m) where -- представитель аппликации трансформера к монаде
    return x = ...
    mx >>= k = ...


2'. Функцию fail нужно реализовать обязательно

Если монада обрабатывает ошибки — реализовать содержательный обработчик:
instance Monad m => MonadFail (MyMonadT m) where
    fail s = ...

Если нет — переадресовать обработку ошибок внутренней монаде:
instance MonadFail m => MonadFail (MyMonadT m) where
    fail = lift . fail


3. Функция lift :: m a -> t m a определена как метод класса типов MonadTrans

class MonadTrans t where
    lift :: Monad m => m a -> t m a

Поднимаем значение из трансформируемой монады в трансформированную, реализуя представителя
instance MonadTrans MyMonadT where
    lift mx = ...

--------------------------------------------------------

Законы класса типов MonadTrans

1. Right Zero – Правый ноль
lift . return ≡ return

2. Left Distribution – Левая дистрибутивность
lift (m >>= k) ≡ lift m >>= (lift . k)

Оба закона значат, что lift не должен вносить никаких эффектов 
-}





--------------------------------------------------------
       {- Реализация трансформера монад MaybeT -}

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}
-- MaybeT :: m (Maybe a) -> MaybeT m a
-- runMaybeT :: MaybeT m a -> m (Maybe a)

instance MonadTrans MaybeT where
    lift :: Functor m => m a -> MaybeT m a
    lift = MaybeT . fmap Just -- :t Just

instance Monad m => Monad (MaybeT m) where
    return :: a -> MaybeT m a
    return = MaybeT . fmap Just . return -- lift . return

    (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    mx >>= k = MaybeT $ do
        v <- runMaybeT mx
        case v of
            Nothing -> return Nothing
            Just y -> runMaybeT (k y)

instance Monad m => MonadFail (MaybeT m) where
    fail :: String -> MaybeT m a
    fail _ = MaybeT $ return Nothing

instance Monad m => Functor (MaybeT m) where
    fmap  = liftM

instance Monad m => Applicative (MaybeT m) where
    pure = return
    (<*>) = ap


mbSt :: MaybeT (StateT Integer Identity) Integer      
mbSt = do 
  lift $ modify (+1)
  a <- lift get
  True <- return $ a >= 3 -- для использования guard (a >= 3) нужен представитель Alternative
  return a
-- runIdentity $ evalStateT (runMaybeT mbSt) 0
-- runIdentity $ evalStateT (runMaybeT mbSt) 2

instance Monad m => Alternative (MaybeT m) where
    empty = MaybeT $ return Nothing
    x <|> y = MaybeT $ do v <- runMaybeT x
                          case v of
                            Nothing -> runMaybeT y
                            Just _  -> return v

instance Monad m => MonadPlus (MaybeT m) -- для msum, mfilter
      
mbSt' :: MaybeT (State Integer) Integer
mbSt' = do lift $ modify (+1)
           a <- lift get
           guard $ a >= 3 --
           return a
-- evalState (runMaybeT mbSt') 0
-- evalState (runMaybeT mbSt') 2

-- Избавимся от подъема стандартных операций вложенной монады:
instance MonadState s m => MonadState s (MaybeT m) where
    get = lift get
    put = lift . put
--
mbSt'' :: MaybeT (State Integer) Integer
mbSt'' = do 
    modify (+1) -- без lift
    a <- get -- без lift
    guard $ a >= 3
    return a
-- runIdentity $ evalStateT (runMaybeT mbSt'') 0
-- runIdentity $ evalStateT (runMaybeT mbSt'') 2
