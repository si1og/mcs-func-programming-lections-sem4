{-# LANGUAGE InstanceSigs #-}

module TrRW where

import Control.Monad.Identity
import Data.Monoid (Sum(..))
import Data.Maybe (fromMaybe)
import Data.Char (toUpper)

--------------------------------------------------------
      {- Реализация трансформера монад ReaderT
                  и сравнение с монадой Reader -}


-- Сравнение типов монады и трансформера монад:

--               свободное возвращаемое значение
--               🠗                              🠗
newtype Reader r a = Reader { runReader :: r -> a }
--             🠕                           🠕
--            параметр связанный с окружением

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
--                🠕                                  🠕
--  параметр внутренней монады; она используется с возвращаемым значением

{-
TrRW.runReader (TrRW.Reader (*2)) 3
TrRW.runReaderT (TrRW.ReaderT (*2)) 3
:t TrRW.runReaderT (TrRW.ReaderT (*2)) 3
                                  🠕
                     сигнатура типа требует монады
            здесь по факту тип (r -> a), нужет тип (r -> m a)
TrRW.runReaderT (TrRW.ReaderT (\x -> [x*2])) 3
                                     🠕
                        например: список - монада



Напишем универсальную функцию которая может запускать любую монаду имея просто стрелку (r -> a)
-}
reader :: Monad m => (r -> a) -> ReaderT r m a
reader f = ReaderT $ return . f
{-
:t TrRW.runReaderT (TrRW.reader (*2)) 3
TrRW.runReaderT (TrRW.reader (*2)) 3 :: IO Int 
TrRW.runReaderT (TrRW.reader (*2)) 3 :: [Int]
TrRW.runReaderT (TrRW.reader (*2)) 3 :: Maybe Int

Больше не нужно напрямую зашивать структуру монады, это теперь будет сделано из типов





Реализуем представителей
    - функторов
    - аппликативных функторов
    - монад
* Можно просто написать представителей монады, остальное выведется автоматически


Функтор для базового типа частично примененной стрелки - композиция
instance Functor ((->) r) where
    fmap :: (a -> b) -> (r -> a) -> (r -> b)
    fmap f g = f . g

Простыми переупаковками можно написать представителя для Reader
-}

instance Functor (Reader r) where
    fmap :: (a -> b) -> Reader r a -> Reader r b -- расширение {-# LANGUAGE InstanceSigs #-}
                                                 -- для возможности использовать конкретный тип (Reader r)
    fmap f x = Reader $ f . runReader x {-
                 🠕      🠕       🠕
                 🠕      🠕   снятие контекста (упаковки)
                 🠕    композиция функции
            восстановкление контекста
TrRW.runReader (fmap (+1) $ TrRW.Reader (*2)) 3

ReaderT аналогичен Reader, но есть дополнительный монадический контекст
-}
instance Functor m => Functor (ReaderT r m) where
    fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
    fmap f x = ReaderT $ fmap f . runReaderT x
{-                         🠕
             для работы с внутренней монадой
             :t fmap f
             :t runReaderT x

TrRW.runReaderT (fmap (+1) $ TrRW.reader (*2)) 3 :: IO Int



Присвоение типов вида ... :: [Int]
Позволяет использовать тривиальные типы, создадим пример с боле сложными структурами и эффектами
    чтение внешнего окружения и создание вычисления в списке из трех элементов
-}
listRead = ReaderT $ \e -> [2, e, e * 3] -- :t listRead
{-
TrRW.runReaderT (fmap (+1) listRead) 4





Аппликативный функтор для Reader и ReaderT
В процессе реализации используем идею аналогичную функтору

instance Applicative ((->) r) where
    pure :: a -> (r -> a)
    pure x e = x
    (<*>) :: (r -> (a -> b)) -> (r -> a) -> (r -> b)
    (<*>) g h e = g e (h e)
-}
instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure = Reader . const
    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    f <*> v = Reader $ \ e -> runReader f e (runReader v e)
-- TrRW.runReader (Reader (+) <*> Reader (^2)) 3

instance Applicative m => Applicative (ReaderT r m) where
    pure :: a -> ReaderT r m a
    pure = ReaderT . const . pure
--                            🠕
--      нужно поднять оборачиваемое значение в контекст монады
    (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
    f <*> v = ReaderT $ \ e -> runReaderT f e <*> runReaderT v e
{-                         :t runReaderT f e  :t runReaderT v e
Т.к. внутри и снаружи аппликативы, то можно реализовать (<*>) через функцию liftA2
    просто используя в качестве функции <*> который протащит функции и значения через вторые обертки самостоятельно
    :t liftA2 (<*>)



Функции обернутые в ReaderT:
-}
listReadFun = ReaderT $ \e -> [const e, (+e)]
{-
TrRW.runReaderT (listReadFun <*> listRead) 4





Монада для Reader и ReaderT

instance Monad ((->) r) where
    (>>=) :: (r -> a) -> (a -> (r -> b)) -> (r -> b)
    f >>= k = \e -> k (f e) e
-}
instance Monad (Reader r) where
    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    m >>= k = Reader $ \e -> 
        let v = runReader m e
        in runReader (k v) e

instance Monad m => Monad (ReaderT r m) where
    (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
    m >>= k = ReaderT $ \e -> do 
--                            🠕
--              вычисление во внутренней монаде
        v <- runReaderT m e
        runReaderT (k v) e
-- TrRW.runReaderT (do {x <- listRead; return ((+1) x)}) 4



{-
Все базовые представители реализованы

Монады могут выставлять интерфейс (функции)
Нужен механизм позволяющий вызывать интерфейс внутренней монады во внешней

Теперь нужно протаскивать интерфейс внутренней монады через внешнюю
Для этого нужна реализация функции lift
    С учетом законов для lift
-}
class MonadTrans t where -- Control.Monad.Trans.Class
    lift :: Monad m => m a -> t m a
--                      🠕       🠕
-- по типам из монады нужно сделать трансформер монады
instance MonadTrans (ReaderT r) where
    lift :: Monad m => m a -> ReaderT r m a
    lift m = ReaderT $ \_ -> m
{-       🠕              🠕
 принимаем монаду и     🠕
собираем стрелку в которой будем игнорировать внешнее окружение 

TrRW.runReaderT (do {x <- listRead; y <- TrRW.lift (replicate 3 10); return (x + y)}) 4
                             🠕                🠕           🠕
                             🠕                🠕           здесь вложенная монада списка
                             🠕                вытаскиваем ее в трансформер
                             здесь список уже на уровне трансформера



Стандартный интерфейс монады Reader и трансформера ReaderT:

- доступ к окружению:
ask :: Reader r r
ask = Reader id
-}
ask :: Monad m => ReaderT r m r
ask = ReaderT return
{-
TrRW.runReaderT (do {e <- TrRW.ask; f <- TrRW.lift [((-) 1), (+1)]; return (f e)}) 4

- доступ к обработанному окружению
asks :: (r -> a) -> Reader r a
asks f = Reader f
-}
asks :: Monad m => (r -> a) -> ReaderT r m a
asks f = ReaderT $ return . f
{-
TrRW.runReaderT (do {e <- TrRW.asks (*2); f <- TrRW.lift [((-) 1), (+1)]; return (f e)}) 4

- локальное изменение окружения
local :: (r -> r) -> Reader r a -> Reader r a
local f x = Reader $ \e -> runReader x (f e)
-}
local :: (r -> r) -> ReaderT r m a -> ReaderT r m a
local f x = ReaderT $ runReaderT x . f
-- TrRW.runReaderT (do {e1 <- TrRW.local (*2) listRead; e2 <- TrRW.ask; return (e1 + e2)}) 4





--------------------------------------------------------
      {- Реализация трансформера монад WriterT -}

newtype Writer w a = Writer { runWriter :: (a,w) }
-- Куда поместить монаду для создания трансформера?

newtype WriterT w m a = WriterT { runWriterT :: m (a,w) }
{-                                             🠕
                                    монада снаружи. Почему?
    после запуска runWriterT мы должны получить доступ ко внутренней монаде (и запустить run? на ней), поэтому она выставляется наружу
!!! никто не запрещает сделать (m a, w), но это неудобно

в трансформере ReaderT было наоборот: r -> m a
                                           🠕
                                    монада внутри. Почему?
    после запуска runReaderT мы должны получить доступ ко внутренней монаде,
        но для обеспечения работы эффекта монады Reader нужно внешнее окружение r, которое мы предоставляем,
        после чего получаем доступ к внутренней монаде

TrRW.runWriterT (TrRW.WriterT $ Just (0,"Hello"))

writer :: (a,w) -> Writer w a
writer = Writer
-}
writer :: Monad m => (a,w) -> WriterT w m a
writer = WriterT . return
{-
:t TrRW.runWriterT (TrRW.writer (0,"Hello"))
TrRW.runWriterT (TrRW.writer (0,"Hello")) :: [(Int,String)]


Вспомогательные функции запуска:
-}
execWriter :: Writer w a -> w
execWriter = snd . runWriter

execWriterT :: Monad m => WriterT w m a -> m w
execWriterT = fmap snd . runWriterT



-- Функтор
instance Functor (Writer w) where
    fmap :: (a -> b) -> Writer w a -> Writer w b
    fmap f = Writer . update . runWriter
        where update ~(y, log) = (f y, log)
{-                   🠕
            неопровержимый образец
    (сопоставление происходит только тогда если 
        вычисления нужны в правой части вычисления.
        Т.е. это ленивая версия Writer, если убрать ~ то будет строгая версия)
-}

instance Functor m => Functor (WriterT w m) where
    fmap :: (a -> b) -> WriterT w m a -> WriterT w m b
    fmap f = WriterT . fmap update . runWriterT
        where update ~(y, log) = (f y, log)
{-
TrRW.runWriter (fmap (^2) $ TrRW.Writer (3,"A"))
TrRW.runWriterT (fmap (^2) $ TrRW.WriterT [(3,"A"), (4,"B")])



Аппликативный функтор
-}
instance Monoid w => Applicative (Writer w) where
    pure :: a -> Writer w a
    pure x = Writer (x, mempty)
    (<*>) :: Writer w (a -> b) -> Writer w a -> Writer w b
    f <*> v = Writer $ update (runWriter f) (runWriter v)
        where update ~(g,w) ~(x,w') = (g x, w `mappend` w')

instance (Monoid w, Applicative m) => Applicative (WriterT w m) where
    pure :: a -> WriterT w m a
    pure x = WriterT $ pure (x, mempty)
    (<*>) :: WriterT w m (a -> b) -> WriterT w m a -> WriterT w m b
    f <*> v = WriterT $ liftA2 update (runWriterT f) (runWriterT v)
        where update ~(g,w) ~(x,w') = (g x, w `mappend` w')
{-
TrRW.runWriter (Writer ((*2), "Hello ") <*> Writer (3, "world"))
TrRW.runWriterT (writer ((*2), "Hello ") <*> writer (3, "world")) :: [(Int, String)]
TrRW.runWriterT (WriterT [((*2), "*2"), ((*3), "*3")] <*> WriterT [(3, " 3"), (4, " 4")])



Монада
-}
instance Monoid w => Monad (Writer w) where
    (>>=) :: Writer w a -> (a -> Writer w b) -> Writer w b
    m >>= k = Writer $ let
        (v, w) = runWriter m
        (v', w') = runWriter (k v)
        in (v', w `mappend` w')

instance (Monoid w, Monad m) => Monad (WriterT w m) where
    (>>=) :: WriterT w m a -> (a -> WriterT w m b) -> WriterT w m b
    m >>= k = WriterT $ do
        ~(v, w) <- runWriterT m
        ~(v', w') <- runWriterT (k v)
        return (v', w `mappend` w')
{-
runWriter $ do {x <- Writer (1, "Hello"); return ((+2) x)}
runWriterT $ do {x <- writer (1, "Hello"); return ((+2) x)} :: [(Int, String)]
runWriterT $ do {x <- WriterT [(1, "Hello"),(3,"world")]; return ((+2) x)} :: [(Int, String)]

Реализация функции fail
Протаскивание ошибки без дополнительной обработки
-}
instance (Monoid w, MonadFail m) => MonadFail (WriterT w m) where
    fail :: String -> WriterT w m a
    fail = WriterT . fail
{-
TrRW.runWriterT $ do {5 <- TrRW.writer (1, "Hello"); return 6} :: [(Int, String)]
TrRW.runWriterT $ do {3 <- TrRW.WriterT [(1, "Hello"),(3,"world")]; return 7} :: [(Int, String)]


Протаскивание внутренней монады наружу
-}
instance (Monoid w) => MonadTrans (WriterT w) where
    lift :: Monad m => m a -> WriterT w m a
    lift m = WriterT $ do
        x <- m
        return (x, mempty)

listWrite = WriterT $ [(1,"one"),(2,"two"),(3,"three")]
{-
TrRW.runWriterT $ do {x <- listWrite; f <- TrRW.lift [(+4),(*5)]; return (f x)}



Стандартный интерфейс WriterT
- запись значения в лог
-}
tell :: Monad m => w -> WriterT w m ()
tell w = writer ((),w)
{-
TrRW.runWriterT $ do {x <- listWrite; f <- TrRW.lift [(+4),(*5)]; tell " hello"; return (f x)}

- сохранение лога внутреннего вычисления и предоставление этого лога пользователю в значении
(можно подглядеть в лог)
-}
listen :: Monad m => WriterT w m a -> WriterT w m (a,w)
listen m = WriterT $ do
    ~(a,w) <- runWriterT m
    return ((a,w), w)
{-
runWriterT (listen listWrite)

- модификация лога
-}
censor :: Monad m => (w -> w) -> WriterT w m a -> WriterT w m a
censor f m = WriterT $ do
    ~(a,w) <- runWriterT m
    return (a, f w)
{-
runWriterT (censor (++ " 123") listWrite)
-}





--------------------------------------------------------
type Vegetable = String
type Price = Double
type Qty = Double
type Cost = Double
type PriceList = [(Vegetable,Price)]

prices :: PriceList
prices = [("Potato",13),("Tomato",55),("Apple",48)]

addVegetable :: Vegetable -> Qty 
               -> WriterT (Sum Cost)
                          (ReaderT PriceList Identity)
                          (Vegetable, Price)
addVegetable veg qty = do
    priceList <- lift ask
    let pr = fromMaybe 0 $ lookup veg priceList
    let cost = qty * pr
    tell $ Sum cost
    return (veg, pr)

runAddVegetable = runIdentity $ runReaderT (runWriterT $ addVegetable "Apple" 100) prices
-- runIdentity $ TrRW.runReaderT (TrRW.runWriterT $ TrRW.addVegetable "Apple" 100) TrRW.prices



logFirstAndRetSecond :: ReaderT [String] (WriterT String Identity) String
logFirstAndRetSecond = do
    el1 <- asks head
    el2 <- asks (map toUpper. head . tail)
    lift $ tell el1
    return el2
-- run...





--------------------------------------------------------
{-

Таблица стандартных трансформеров

В библиотеках mtl/transformers определены трансформеры

Монада  Трансформер Исходный тип    Тип трансформера
Reader  ReaderT     r -> a          r -> m a
Writer  WriterT     (a,w)           m (a,w)
State   StateT      s -> (a,s)      s -> m (a,s)
Except  ExceptT     Either e a      m (Either e a)
Cont    ContT       (a -> r) -> r   (a -> m r) -> m r

Монады определены через трансформеры:
type Reader r = ReaderT r Identity
type Writer w = WriterT w Identity
type State  s = StateT  s Identity
type Except e = ExceptT e Identity
type Cont   r = ContT   r Identity

--------------------------------------------------------

Некоммутативность трансформеров:

Применение StateT к монаде Except даёт функцию трансформирования типа s -> Either e (a, s).
    Если ошибка обозначает, что и состояние и значение не могут быть вычислены, то нам следует применять StateT к Except.

Применение ExceptT к монаде State даёт функцию трансформирования типа s -> (Either e a, s).
    Если ошибка обозначает, что только значение не может быть вычислено, но состояние при этом не «портится», то нам следует применять ExceptT к State.

-}
