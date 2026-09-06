-------------------------------------------------------------------------
-- Лекция 06 Полугруппа. Моноид. Функтор. Аппликативный функтор. Stack --
-------------------------------------------------------------------------

-- {-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative qualified as A
import Data.Coerce qualified as C
import Data.Foldable qualified as FO
import Data.Functor qualified as FU
import Data.List.NonEmpty qualified as NE
import Data.Monoid qualified as M
import Data.Semigroup qualified as S

--------------------------------------------------------
{-
Полугруппа — это множество с ассоциативной бинарной операцией над ним
Бинарная операция - математическая операция, которая принимает два аргумента и возвращает один результат
Для полугруппы результат бинарной операции должен быть элементом множества
Ассоциативность - возможность последовательного применения бинарной операции в произвольном порядке (a+b)+c = a+(b+c)
Например:
    сложение, вычитание, умножение, деление - бинарные операции
    множество натуральных чисел N и операция сложения (+) образуют полугруппу
    множество натуральных чисел N и операция вычитания (-) НЕ образуют полугруппу
    множество целых чисел Z и операция вычитания (-) НЕ образуют полугруппу

Класс типов Semigroup
    import qualified Data.Semigroup as S,
    import qualified Data.List.NonEmpty as NE

class Semigroup a where
  (<>) :: a -> a -> a
  sconcat :: GHC.Internal.Base.NonEmpty a -> a
  stimes :: Integral b => b -> a -> a
  {-# MINIMAL (<>) | sconcat #-}

Закон для полугруппы (ассоциативность):
    (x <> y) <> z ≡ x <> (y <> z)

Примеры:
Список — полугруппа относительно конкатенации (++).
instance Semigroup [a] where
    (<>) = (++)

[1,2,3] <> [4,5,6]
[1,2,3] ++ [4,5,6]

Числа Int - полугруппа относительно сложения и умножения
(1 :: Int) <> (2 :: Int)
(1 :: Int) + (2 :: Int)
(1 :: Int) * (2 :: Int)

S.stimes 11 "ab" -- (("ab" <> "ab") <> ("ab" <> "ab")) <> (("ab" <> "ab") <> ("ab" <> "ab")) <> ("ab" <> "ab") <> "ab"

Полугруппа не обязана иметь нейтральный элемент, поэтому нужно гарантировать наличие не пустых элементов
:i NE.NonEmpty

data NonEmpty a = a :| [a] — непустой список элементов типа a (гарантированно содержит хотя бы один элемент)
           Оператор :| создает непустой список (NonEmpty) (объединяет первый элемент с остальными элементами списка)

:t ("AB" NE.:| ["CDE","FG"])

S.sconcat $ "AB" NE.:| ["CDE","FG"]

-}

data Color = Red | Yellow | Blue | Green | Purple | Orange | Brown | Alpha deriving (Show, Eq)

instance Semigroup Color where
  (<>) Red Blue = Purple
  (<>) Blue Red = Purple
  (<>) Yellow Blue = Green
  (<>) Blue Yellow = Green
  (<>) Yellow Red = Orange
  (<>) Red Yellow = Orange
  (<>) Red Alpha = Red
  (<>) Yellow Alpha = Yellow
  (<>) Blue Alpha = Blue
  (<>) Green Alpha = Green
  (<>) Purple Alpha = Purple
  (<>) Orange Alpha = Orange
  (<>) Brown Alpha = Brown
  (<>) Alpha Red = Red
  (<>) Alpha Yellow = Yellow
  (<>) Alpha Blue = Blue
  (<>) Alpha Green = Green
  (<>) Alpha Purple = Purple
  (<>) Alpha Orange = Orange
  (<>) Alpha Brown = Brown
  (<>) a b = if a == b then a else Brown -- выполняется ли закон Semigroup?
  -- проверяется руками
  {-
  Проверка выполнения закона полугрупп:
  Red <> Yellow
  Red <> Blue
  Green <> Purple
  (Green <> Blue) <> Yellow
  Green <> (Blue <> Yellow)

  instance Semigroup Color where
    (<>) Red    Blue    = Purple
    (<>) Blue   Red     = Purple
    (<>) Yellow Blue    = Green
    (<>) Blue   Yellow  = Green
    (<>) Yellow Red     = Orange
    (<>) Red    Yellow  = Orange

    (<>) Red    Alpha   = Red
    (<>) Yellow Alpha   = Yellow
    (<>) Blue   Alpha   = Blue
    (<>) Green  Alpha   = Green
    (<>) Purple Alpha   = Purple
    (<>) Orange Alpha   = Orange
    (<>) Brown  Alpha   = Brown
    (<>) Alpha  Red     = Red
    (<>) Alpha  Yellow  = Yellow
    (<>) Alpha  Blue    = Blue
    (<>) Alpha  Green   = Green
    (<>) Alpha  Purple  = Purple
    (<>) Alpha  Orange  = Orange
    (<>) Alpha  Brown   = Brown

    (<>) a b | a == b = a
             | all (`elem` [Red, Blue, Purple]) [a, b]   = Purple
             | all (`elem` [Blue, Yellow, Green]) [a, b] = Green
             | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange
             | otherwise = Brown

  Проверка выполнения закона полугрупп:
  (Green <> Blue) <> Yellow
  Green <> (Blue <> Yellow)
  -}

--------------------------------------------------------
{-
Моноид — это множество с ассоциативной бинарной операцией над ним и нейтральным элементом для этой операции
Например:
    множество целых чисел, операций сложения и нулем в качестве нейтрального элемента

Класс типов Monoid
    import qualified Data.Monoid as M
    import qualified Data.Coerce as C

class Semigroup a => Monoid a where
    mempty :: a

    mappend :: a -> a -> a
    mappend = (<>)

    mconcat :: [a] -> a
    mconcat = foldr mappend mempty

Законы для моноида:
    mempty <> x ≡ x
    x <> mempty ≡ x
    (x `mappend` y) `mappend` z ≡ x `mappend` (y `mappend` z)

Список — моноид относительно конкатенации (++), нейтральный элемент — это пустой список.
instance Semigroup [a] where
    (<>) = (++)
instance Monoid [a] where
    mempty = []
    mconcat = concat

mconcat ["asd", "fgh", "jhkl"]

Bool - моноид относительно конъюнкции (&&) и дизъюнкции (||)

Библиотечная реализация моноида для Bool:

newtype All = All { getAll :: Bool } deriving (Eq, Ord, Read, Show, Bounded)
instance Semigroup All where
    All x <> All y = All (x && y)
instance Monoid All where
    mempty = True

newtype Any = Any { getAny :: Bool } deriving (Eq, Ord, Read, Show, Bounded)
instance Semigroup Any where
    Any x <> Any y = Any (x || y)
instance Monoid All where
    mempty = False

M.Any True
M.getAny (M.Any True)
M.getAny . M.mconcat . map M.Any $ [False,False,True]
M.getAll . M.mconcat . map M.All $ [False,False,True]

Числа (Num) - моноид относительно:
        - сложения (нейтральный элемент - 0)
        - умножения (нейтральный элемент - 1)
        - min (нейтральный элемент - maxBound) -- не для всех
        - max (нейтральный элемент - minBound) -- не для всех

Библиотечная реализация моноида для Num:

newtype Sum a = Sum { getSum :: a } deriving (Eq, Ord, Read, Show, Bounded, Num)
instance Num a => Semigroup (Sum a) where
    Sum x <> Sum y = Sum (x + y)
instance Num a => Monoid (Sum a) where
    mempty = Sum 0

M.Sum 4
M.getSum (M.Sum 4)
M.Sum 3 <> M.Sum 2
M.Sum 2 * M.Sum 3 - M.Sum 5

newtype Product a = Product { getProduct :: a } deriving (Eq, Ord, Read, Show, Bounded, Num)
instance Num a => Semigroup (Product a) where
    (<>) = coerce ((*) :: a -> a -> a) -- Data.Coerce
instance Num a => Monoid (Product a) where
    mempty = Product 1

M.Product 3 <> M.Product 2

newtype Min a = Min { getMin :: a } deriving (Eq, Ord, Read, Show, Bounded)
instance Ord a => Semigroup (Min a) where
    (<>) = coerce (min :: a -> a -> a)
    stimes = stimesIdempotent
instance (Ord a, Bounded a) => Monoid (Min a) where
    mempty = maxBound

S.Min "Hello" <> S.Min "Hi"
(S.getMin . M.mconcat . map S.Min) [7,3,2,12] :: Int
(S.getMin . M.mconcat . map S.Min) [] :: Int

-- S.Min и S.Max вместо M.Min и M.Max
    т.к. не все (Min a) можно сделать Monoid
    например Int (имеет min и max) и Integer (не имеет min и max)
из info:
instance (Ord a, Bounded a) => Monoid (S.Max a)
instance (Ord a, Bounded a) => Monoid (S.Min a)
           🠕      🠕
          дополнительные ограничения на тип (см. info Int Integer)
-}

-- Определим представителя класса типов Monoid для Color
-- instance Monoid Color where
--    ...

-- M.mappend Red M.mempty
-- M.mconcat [Red, M.mempty, Yellow]

--------------------------------------------------------
{-
Функтор - тип, который поддерживает операцию применения функции к значению, находящемуся внутри контекста, без изменения структуры контекста
Поднимает стрелку на уровень контейнера

Функтор реализует возможность применять чистые функции к значению в контексте

Пример с упаковкой:
-}
data Box a = Box a deriving (Show, Eq, Ord, Read, Bounded)

{-
Box 3
Box 3 == Box 5
Box 3 + Box 5 -- ?

Реализуем функцию которая может выполнить операцию Box 3 + Box 5
и сделаем ее максимально универсальной
-}

-- applyBoxes ::
-- applyBoxes

{-
applyBoxes (+) (Box 4) (Box 6)
applyBoxes (+1) (Box 4)
temp = applyBoxes (+) (Box 4)
temp (Box 6)

Класс типов Functor
    import qualified Data.Functor as FU

class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  {-# MINIMAL fmap #-}

map  :: (a -> b) -> [a] -> [b]
fmap :: (a -> b) -> f a -> f b

(<$>) = fmap
($>) = flip (<$)

Just 42 $> "foo"
Nothing $> "foo"
Nothing <$ "foo"

(<&>) :: Functor f => f a -> (a -> b) -> f b
xs <&> f = f <$> xs

(+10) <$> (^2) <$> [1,2,3]
[1,2,3] FU.<&> (^2) FU.<&> (+10)

Законы функтора:
    fmap id = id (сохранение тождественного морфизма)
    fmap (f . g) = fmap f $ fmap g (сохранение композиции)
-}
-- Определим представителя Functor для Box
-- instance Functor Box where
--    ...

instance Functor Box where
  fmap f (Box x) = Box (f x)

{-

Проверка законов функтора:
fmap (\x -> x) (Box 3) ≡ (\x -> x) (Box 3)
fmap ((+1) . (*2)) (Box 4) ≡ fmap (+1) $ fmap (*2) (Box 4)

fmap (+1) (Box 4)

Библиотечные представители класса типов Functor:

Список - функтор (обработка каждого элемента списка)

instance Functor [] where
    fmap f [] = []
    fmap f (x:xs) = f x : fmap f xs

fmap (+1) [1, 2, 3]

Maybe - функтор (обработка элемента в контейнере Just и сохранение эффекта Nothing)

instance Functor Maybe where
    fmap f Nothing = Nothing
    fmap f (Just x) = Just (f x)

fmap (+1) (Just 5)
fmap (+1) Nothing
(+1) Nothing

Представители Functor для двухпараметрических типов (:: * -> * -> *)

instance Functor (Either e) where
    fmap :: (a -> b) -> Either e a -> Either e b
    fmap _ (Left x) = Left x
    fmap g (Right y) = Right (g y)

fmap (+1) (Left 5)
fmap (+1) (Right 5)

instance Functor ((,) s) where
    fmap :: (a -> b) -> (s,a) -> (s,b)
    fmap g (x,y) = (x, g y)

fmap (+1) (1,2)

instance Functor ((->) e) where
    fmap :: (a -> b) -> (->) e a -> (->) e b
         -- (a -> b) -> (e -> a) -> (e -> b)
    fmap = (.) -- f . g = f (g x) -- композиция
Контейнером здесь является результат будущей функции

:t (fmap (+1) (*2))
           🠕    🠕
           🠕    функция контейнер (применяется первой)
           функция-аргумент (применяется второй)
(fmap (+1) (*2)) 5
(+1) <$> (*2) $ 5
((+1) <$> (*2)) 5

f <$> g <$> xs ≡ (f <$> g) <$> xs ≡ (f . g) <$> xs ≡ fmap (f . g) xs

Не все типы можно сделать представителями функтора (сложности возникают со стрелочными типами):

-- Эндоморфизм (функция, которая принимает и возвращает значение одного и того же типа):
newtype Endo a = Endo { appEndo :: a -> a }
instance Functor Endo where
    fmap :: (a -> b) -> Endo a -> Endo b
         -- (a -> b) -> (a -> a) -> (b -> b)
             🠕           🠕
             🠕           тут на входе тип a и на выходе тип a
             тут на входе тип a и на выходе тип b
                                     при таких функциях результат будет (a -> b)
                                     но по типам должно быть Endo b - это тип (b -> b)
    fmap _ (Endo _) = Endo id
-}

--------------------------------------------------------
{-
Свертки полезны при работе со списками, но можно обобщить списки до произвольных сворачиваемых структур
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
                  🠕                          🠕
                  🠕                          это может быть список, но его можно обобщить до любого контейнера
                  контекст контейнера

Класс Foldable
    import qualified Data.Foldable as FO

Как обобщить контейнер для использования свертки?

class Foldable t where
    fold :: Monoid m => t m -> m
    fold = foldMap id

    foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
    foldMap f = foldr (mappend . f) mempty
--                               f :: a -> m
--                     mappend :: m -> (m -> m)
--                    (mappend . f) :: a -> (m -> m)
--              foldr ::              (a ->  b -> b) -> b -> t a -> b
--                                  mempty ::           m
--  foldMap ::                        (a ->       m) ->      t a -> m

    foldr, foldr' :: (a -> b -> b) -> b -> t a -> b
    foldr f z t = appEndo (foldMap (Endo . f) t) z

    foldl, foldl' :: (a -> b -> a) -> a -> t b -> a
    foldl f z t = appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z

    toList :: t a -> [a] -- любой контейнер Foldable может быть сведен к списку

    null :: t a -> Bool
    null = foldr (\_ _ -> False) True

    length :: t a -> Int
    length = foldl' (\n _ -> n + 1) 0

    elem :: Eq a => a -> t a -> Bool
    ...
    {-# MINIMAL foldMap | foldr #-}

FO.fold [M.Sum 1, M.Sum 2, M.Sum 3, M.Sum 4]
M.getSum $ foldMap (M.Sum . (^2)) [1, 2, 3, 4]
FO.foldr (+) 0 [1, 2, 3, 4]
FO.foldl (*) 1 [1, 2, 3, 4]
FO.foldr1 max [1, 2, 3, 4]
FO.foldl1 min [1, 2, 3, 4]

Законы Foldable:
    foldr f z t ≡ appEndo (foldMap (Endo . f) t ) z
    foldl f z t ≡ appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z
    fold ≡ foldMap id
    length ≡ getSum . foldMap (Sum . const 1)
    sum ≡ getSum . foldMap Sum
    product ≡ getProduct . foldMap Product
    minimum ≡ getMin . foldMap Min
    maximum ≡ getMax . foldMap Max
    foldr f z ≡ foldr f z . toList
    foldl f z ≡ foldl f z . toList

Представители базовых типов для класса типов Foldable:

instance Foldable Maybe where
    foldr _ z Nothing = z
    foldr f z (Just x) = f x z

FO.foldr (+) 1 (Just 3)
FO.foldr (+) 1 Nothing
FO.foldr (flip $ FO.foldr (+)) 1 [Just 3, Nothing, Just 4, Just 5]
             :t (FO.foldr (+))
      :t (flip $ FO.foldr (+))

Двухпараметрические представители Foldable:

instance Foldable (Either a) where
    foldMap :: (b -> m) -> Either a b -> m
    foldMap _ (Left _) = mempty
    foldMap f (Right y) = f y

    foldr _ z (Left _) = z
    foldr f z (Right y) = f y z

    length (Left _) = 0
    length (Right _) = 1

    null = isLeft

FO.maximum (Right 37)
FO.maximum (Left 37)

instance Foldable ((,) a) where
    foldMap :: (b -> m) -> (,) a b -> m
    foldMap f (_,y) = f y

    foldr f z (_,y) = f y z

    length _ = 1

    null _ = False

FO.foldr (+) 5 ("Answer",37)
FO.maximum (100,42)

-}
-- Определим представителя Foldable для Box
-- instance Foldable Box where
--    ...

--------------------------------------------------------
{-
Как положить значение в контейнер?
                    ... упаковку?

Как поднять значение в контекст?
                   ... вычислительный контекст?

Нужна функция, которая может универсальным способом взять чистое значение и поместить его в контекст без дополнительных эффектов

Такая функция должна иметь тип:
pure :: a -> f a
        🠕    🠕 🠕
        🠕    🠕 чистое значение в контейнере
        🠕    контейнер (контекст)
        чистое значение

Возьмем существующие базовые типы и напишем реализацию этой функции:

Тип Maybe - эффект отсутствующего значения
instance ? Maybe where
    pure x = ...

Тип [a] - эффект множества значений
instance ? [] where
    pure x = ...

Тип Either - эффект завершения с ошибкой
instance ? (Either e) where
    pure x = ...

Тип частично примененной функциональной стрелки - эффект чтения из внешнего окружения
    (отсутствие эффекта - игнорирование внешнего окружения, т.е. константная функция)
instance ? ((->) e) where
    pure x = \_ -> x

Тип пары значений - эффект записи в лог
    (отсутвтвие эффекта - пустой лог)
instance ? ((,) s) where
    pure x = (mempty, x)

Закон связи fmap и pure:
fmap g . pure = pure . g

:t (fmap (\x -> x) . pure)
:t (pure . (\x -> x))

a -> pure -> f a -> fmap g -> f b
a -> g    -> b   -> pure   -> f b
-}

--------------------------------------------------------
{-
Как работать с частично примененной функцией в контексте?

:t (FU.fmap (+) (Box 5))
FU.fmap (FU.fmap (+) (Box 5)) (Box 2) -- что не так?

С библиотечными типами та же проблема:
(+1) <$> Just 2
:t (Just (+))
:t ((+) <$> Just 2)
(+) <$> Just 2 <$> Just 3

Функция которая может принимать чистую функцию двух аргументов и пару обернутых аргументов:
fmap2 :: (a -> b -> c) -> f a -> f b -> f c
fmap2 g as bs = fmap g as `ap` bs
                fmap g as :: f (b -> c)
                          `ap` :: f (b -> c) -> f b -> f c
    Здесь нужна реализация функции `ap` ...

Что если мне нужна функция большего количества аргументов?
fmap3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d
fmap3 g as bs cs = (fmap g as `ap` bs) `ap` cs
                               🠕        🠕
                            Здесь тоже нужна реализация функции `ap` ...
                            И у нее тот же тип:
                                `ap` :: f (b -> c) -> f b -> f c

Универсальную `ap` невозможно получить для произвольного функтора

Таким образом для работы с функциями в контексте нужно написать реализацию функции `ap` для каждого типа

  функция   значение  результат
     🠗          🠗      🠗
f (b -> c) -> f b -> f c -- стрелка в контексте примененная к значению в контексте
🠕             🠕      🠕
🠕______контекст______🠕

Класс типов Applicative:
    import qualified Control.Applicative as A

class Functor f => Applicative f where
    pure :: a -> f a

    (<*>) :: f (a -> b) -> f a -> f b
    (<*>) = liftA2 id

    liftA2 :: (a -> b -> c) -> f a -> f b -> f c
    liftA2 g a b = g <$> a <*> b

    (*>) :: f a -> f b -> f b
    a1 *> a2 = (id <$ a1) <*> a2

    (<*) :: f a -> f b -> f a
    (<*) = liftA2 const

    {-# MINIMAL pure, ((<*>) | liftA2) #-}

(+) <$> Just 2 <*> Just 3
pure (+) <$> Just 2 <*> Just 3
pure (+) <*> Just 2 <*> Just 3
-}
-- Определим представителя Applicative для Box
instance Applicative Box where
  pure x = Box x
  Box f <*> Box x = Box (f x)

{-
Закон связи Applicative и Functor:
fmap g xs ≡ pure g <*> xs
            pure должен быть «без эффектным»

Законы Applicative:
    Identity:     pure id <*> v ≡ v
    Homomorphism: pure g <*> pure x ≡ pure (g x)
    Interchange:  u <*> pure x ≡ pure ($ x) <*> u
    Composition:  pure (.) <*> u <*> v <*> x ≡ u <*> (v <*> x)

Представители базовых типов для класса типов Applicative:

instance Applicative Maybe where
    pure = Just

    Nothing <*> _ = Nothing
    (Just g) <*> x = fmap g x

Just (+2) <*> Just 5
Just (+2) <*> Nothing
Just (+) <*> Just 2 <*> Just 5

Списки могут работать с двумя подходами:
    декартово произведение (каждый с каждым)
    попарно
-}
fs = [(2 *), (3 +), (4 -)]

xs = [1, 2]

{-
Список — контекст, задающий множественные результаты недетерминированного вычисления:
fs <*> xs → [(2*)1,(2*)2,(3+)1,(3+)2,(4-)1,(4-)2] → [2,4,4,5,3,2]
Список — это коллекция упорядоченных элементов:
fs <*> xs → [(2*)1,(3+)2] → [2,5]

Как работает контекст списков в аппликативных функторах по умолчанию?
[(2*),(3+),(4-)] <*> [1,2]

Стандартный представитель списков для аппликатива:

instance Applicative [] where
    pure x = [x]
    gs <*> xs = [ g x | g <- gs, x <- xs ]

Представитель списка как коллекция упорядоченных элементов:

newtype ZipList a = ZipList { getZipList :: [a] }

instance Functor ZipList where
    fmap f (ZipList xs) = ZipList (map f xs)

instance Applicative ZipList where
    pure x = ZipList (repeat x)
    ZipList gs <*> ZipList xs = ZipList (zipWith ($) gs xs)

A.getZipList $ A.ZipList fs <*> A.ZipList xs

instance Monoid s => Applicative ((,) s) where
    pure x = (mempty, x)
    (u, f) <*> (v, x) = (u <> v, f x)

("Answer to ",(*)) <*> ("the Ultimate ",6) <*> ("Question",7)
-}

--------------------------------------------------------
{-
($)         ::                    (a -> b) ->    a ->    b
map         ::                    (a -> b) -> [] a -> [] b
(<$>), fmap :: Functor f     =>   (a -> b) -> f  a -> f  b
(<*>)       :: Applicative f => f (a -> b) -> f  a -> f  b
pure        :: Applicative f =>                  a -> f  a
-}
--------------------------------------------------------

--------------------------------------------------------
{-
Stack - инструмент сборки и управления проектами на Haskell
- Автоматическое изолирование версии компилятора GHC для конкретного проекта
Можно использовать разные версии компилятора для разных проектов без конфликтов в системой
- Snapshots (снимки состояния) - набор версий пакетов, которые гарантированно совместимы друг с другом и с версией компилятором
Snapshots скачиваются из репозитория Stackage
Снижает вероятность возникновения конфликтов зависимостей
- Воспроизводимость сборки. Изменить план сборки можно только осознанно (или просто сломать)
- Sandboxing (изоляция проекта). У каждого проекта собственное изолированное окружение, что избавляет от вероятности "загрязнения" глобального пространства пакетов
- Автоматическая сборка проекта и скачивание зависимостей
- Использует инфраструктуру Cabal + LTS сники библиотек

-------------------------------
-- Создание проекта в stack: --
-------------------------------

1. Создание проекта по шаблону:
stack new myStackProject
cd myStackProject

Итог после выполнения п.1:
myStackProject
    |- app
    |   |- Main.hs
    |- src
    |   |- Lib.hs
    |- test
    |   |- Spec.hs
    ...
    |- myStackProject.cabal -- полный файл описания пакета (генерируется из package.yaml)
    |- package.yaml         -- упрощенное описание пакета (hpack)
                               описание пакета
                               список зависимостей (dependencies)
    |- Setup.hs             -- скрипт для кастомизация сборки (по умолчанию использует Cabal)
    |- stack.yaml           -- конфигурация проекта для Stack
                               здесь указан resolver (snapshot) проекта

2. Сборка проекта. Stack автоматически анализирует зависимости, скачивает пакеты из Stackage, кэширует пакеты и версию ghc и компилирует проект:
stack setup -- опционально
stack build

3. Запуск проекта
stack exec myStackProject-exe

имя исполняемого файла находится в файле .cabal в поле executable

stack run -- сборка и запуск проекта

~. Запуск интерактивного режима для удобства разработки (Read-Eval-Print Loop)
stack repl
stack ghci

-- ! загрузится ghci, но в этом случае она будет содержать в себе все функции, библиотеки, модули и пр.

~. Режим "горячей" пере сборки во время разработки
stack build --file-watch
(запускается в отдельном терминале)

-------------------------------
Добавление зависимостей в проект:

Заходим в файл package.yaml
    Находим секцию
        dependencies
    Добавляем пакеты, например text
        - text >=1.2.5
Используем функционал библиотеки в коде
Пересобираем проект

Очистка артефактов разработки (если что-то пошло совсем не так, вместо пересоздания проекта):
stack clean -- удаляет артефакты сборки
stack purge -- удаляет все сгенерированные файлы включая .stack-work

Узнать что и откуда будет скачено в процессе следующей сборки без непосредственной сборки:
stack build --dry-run

Сборка проекта с использованием скачанных проектов и игнорирование снэпшотов и дополнительных зависимостей:
stack build --only-locals
(если проект уже содержит в себе все зависимости)

Список полезных флагов для сборки:
https://docs.haskellstack.org/en/stable/commands/build_command/

Запуск тестов проекта:
stack test
-------------------------------

-}
