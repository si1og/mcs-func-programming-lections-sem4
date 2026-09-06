---------------------------------------------------------------------
-- Лекция 05 Ленивые вычисления. Форсирование. Классы типов. Cabal --
---------------------------------------------------------------------

module Lec05 where

import Data.List
import Data.Ord

{-
Ленивые вычисления (ленивая семантика)
В Haskell нет строго последовательность записи функций, они выполняются только при необходимости
Т.е. только при необходимости использовать результат работы функции для получения результата в другой функции

Используя отложенные вычисления можно пользоваться расходимостями, бесконечностями и неопределенностями
Например:
-}

-- Расходящееся вычисление:
myDivergentCalc n = 1 + myDivergentCalc n

-- Неопределенность (расходимость):
myUndefined = undefined

-- Бесконечность:
myInf = [1 ..]

-- Ошибки:
myError = error "ERROR"

{-
Все эти вычисления валидны, но приводят к проблемам при прямом вызове
НО их все равно можно использовать для продуктивных вычислений или

Сопоставление с образцом - энергичная операция,
                           происходит сверху вниз, слева направо

Механизм сопоставления с образцом и ленивые вычисления могут элиминировать вычисления
-}

-- bar :: ...
bar 1 2 = 3
bar 0 _ = 5

{-
Какие результаты получатся при следующем наборе аргументов:
bar 0 7         -- успех
bar 2 1         -- расходимость (когда она наступила?)
bar 1 (5-3)     -- вычисления по необходимости
bar 1 undefined -- расходимость (когда она наступила?)
bar 0 undefined -- элиминация расходимости

Haskell по умолчанию использует подход вызов-по-необходимости
т.е. по умолчанию нет форсирования вычислений даже при вызове функций
-}

-- Функции могут быть не строгими по аргументу:
f x = 1

-- f undefined

g x = undefined

-- g 1
-- g
-- :t g

-- Функции могут быть не строгими по аргументу:
h x = x

-- h undefined

k a = if a > 0 then 1 else undefined

-- k 3
-- k (-1)

{-
thunk - любое еще не вычисленное выражение
        хранится в памяти как нужное, но пока не вычисленное выражение;
        при первом обращении, thunk вычисляется и хранятся как результат и подменяет выражение по необходимости

let x = 1 + 2 in x + x + x
    ↑            ↑   ↑   ↑
    ↑ здесь ничего не вычисляется, просто формируется thunk
    ↑   ↑   ↑
    ↑ здесь вызывается thunk, выражение ВЫЧИСЛЯЕТСЯ и заменяется на результат
    ↑   ↑
    ↑ здесь ничего не вычисляется, просто подставляется результат
    ↑
    ↑ здесь ничего не вычисляется, просто подставляется результат

Управление последовательностью вычислений реализуется через использование отложенных и форсированных вычислений
- если все всегда откладывать, то вычисления со строгой последовательностью действий очень сложно реализовать (например чтение файлов)
- если все всегда форсировать, то вычисления с бесконечностями и расходимостями не получится использовать (например генераторы)
-
Форсирование вычислений в Haskell реализуется функцией seq
Как функция работает с разными аргументами:

seq 3 4
seq undefined 4
seq (k (-1)) 3
seq (undefined,1) 4 <- не расх
seq (\x -> undefined) 4 <- не расх

// а каких-то случаях дохожу до undefined, а в каких-то нет

Почему результаты такие?
:t seq -- форсирование первого аргумента
Форсирование до конструктора, то есть вычисления все равно не уходит до самого конца (финальный результат не получен)

Зачем вообще такое вычисление? В чем оно помогает?
:t ($!) -- форсирование аппликации (вызов по значению)
// ($!) :: (a -> b) -> a -> b

f $! x = x `seq` f x

Как будут работать простейшие функции не строгие по аргументу с форсированием
f undefined -> 1
f $ undefined -> 1
f $! undefined -> расходимость

Пример использования seq в рекурсивных функциях:
-}

factorial n = helper 1 n
  where
    helper acc k
      | k > 1 = helper (acc * k) (k - 1)
      | otherwise = acc

-- ( ... ((1 * n) * (n - 1)) * (n - 2) * ... * 2) -- thunk для финального acc
-- вызов (отложенные вычисления)
-- иногда компилятор может ускорить нам вычисления
-- !но: это наша ответственность

factorial' n = helper 1 n
  where
    helper acc k
      | k > 1 = (helper $! acc * k) (k - 1) -- форсирование вычисления аргументов acc
      | otherwise = acc

-- будет всегда число на рекурсивный вызов

-- Использование продуктивных расходимостей:
fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci) -- !warning!
-- fibonacci
-- take 15 fibonacci
-- drop 15 fibonacci

{-
Функция iterate генерирует значения, применяя к значению функцию для получения второго значения,
затем применяя ту же самую функцию к этому второму значению для получения третьего значения, и т. д.

iterate f x = [ x, f x, f (f x), f (f (f x)), ... ]
-}

fibonacci2 :: [Integer]
fibonacci2 = map fst $ iterate (\(n, n1) -> (n1, n + n1)) (0, 1)

-- fibonacci2 !! 20
--             ↑ - обращение к конернтному элементу (ухудшает читаемость и замедляет работу программы)

{-
Генераторы списков (list comprehension)
Описывают правила построения списков
-}
myListComp = [1 .. 10] -- числа от 1 до 10 с шагом 1

myListCompStep = [1, 4 .. 50] -- числа от 1 до 50 с шагом 3
-- // [1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46,49]

myListCompLet = ['A' .. 'z']

-- // "ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz"

{-
Как генерировать списки с использованием правил:

Перебираем список через синтаксис генератора:

  структура элементов финального списка
  ↓ разделитель результата и исходных данных и правил
  ↓ ↓
[ x | x <- [1..10] ]
      ↑       ↑
      ↑       список
      элемент обрабатываемого списка

    можно добавить обработку элементов списка
    ↓
[ x * 2 | x <- myListComp ]
               ↑
               используются переменные

     структура может быть любой сложности
     ↓
[ [(x,y)] | x <- "ASD", y <- "zx"]
            ↑           ↑
            в качестве исходных данных используются несколько списков

[ (x,y) | x <- [1..3], y <- [1..x] ]
          ↑                     ↑
          могут быть заданы внутренние зависимости

[ (a,b,c) | a<-myListComp, b<-[1..a], c<-myListComp, a^2 + b^2 == c^2 ]
                                                                ↑
                                                                условия создания элементов списка
-- //  [(4,3,5),(8,6,10)]

Генераторы можно задать любой генератор как map и filter:
[ x^2 | x <- myListComp, even x ]
map (^2) (filter even myListComp)
-}

--------------------------------------------------------
{-
Класс типов - это именованный набор имён функций с сигнатурами, параметризованными общим типовым параметром

Как задать класс типов:

  🠗 ------- ключевые слова ------- 🠗
class TypeClassName typeParameter where
          ↑             ↑ переменная типа (типовой параметр) // обычно обозначается одной буквой
          ↑ имя класса типа
  functionSignatureOne :: typeParameter -> SomeExistingTypeOne
      ↑
      сигнатура функции задаваемого класса типов
      ↓
  functionSignatureTwo :: typeParameter -> typeParameter -> SomeExistingTypeTwo
  ...

Определение класса типов задает интерфейсы
Для того чтобы пользоваться классами типов нужны представители (экземпляры)

Как написать представителя класса типов:

Задаем тип свой тип:
data MyTypeName = MyDataConstructor

  🠗 ------- ключчевые слова ------- 🠗
instance TypeClassName MyTypeName where
              ↑             🠕 тип представителя которого пишем
              🠕 имя класса типа
  functionSignatureOne MyDataConstructor = FunctionBodyOne
      🠕
      содержательная реализация функций
      🠗
  functionSignatureTwo MyDataConstructor MyDataConstructor = FunctionBodyTwo

functionSignatureOne, functionSignatureTwo - имена функций класса типов TypeClassName

Пример из стандартной библиотеки:
:i Eq -- запрос информации о классе типов:
class Eq a (типовой параметр) where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  {-# MINIMAL (==) | (/=) #-}

Нет необходимости писать реализации всех интерфейсов для класса типов
Можно реализовать только то, что перечислено в MINIMAL
  в данном случае либо (==) либо (/=)

:t (==)
Имя класса типов задает ограничение, называемое контекстом:
(==) :: Eq a => a -> a -> Bool

В качестве примера возьмем Bool:

data Bool = True | False

instance Eq Bool where -- представитель Eq для типа данных Bool
    True == True = True
    False == False = True
    _ == _ = False
    x /= y = not (x == y)

Реализуем тип игральной кости и представителей основных классов типов для нее:
(будем делать все вручную)
-}

data SixSidedDie' = S1' | S2' | S3' | S4' | S5' | S6'

mySpan :: (a -> Bool) -> [a] -> ([a], [a])
mySpan f xs = foldr (\y (as, bs) -> if f y then (y : as, bs) else ([], y : as ++ bs)) ([], []) xs

{-
Класс типов Show
Преобразует значение в строку и может вывести ее на экран
:i Show
...
instance Show SixSidedDie' -- Defined at lec_05.hs:290:10
...
Представитель Show:
-}
instance Show SixSidedDie' where
  show S1' = "One"
  show S2' = "Two"
  show S3' = "Three"
  show S4' = "Four"
  show S5' = "Five"
  show S6' = "Six"

myDie = S6'

-- myDie

{-
Класс типов Read
Обратное преобразование строк в значения
-}
instance Read SixSidedDie' where
  readsPrec _ str = case str of
    'r' : 'o' : 'l' : 'l' : ' ' : 'o' : 'n' : 'e' : rest -> [(S1', rest)]
    'r' : 'o' : 'l' : 'l' : ' ' : 't' : 'w' : 'o' : rest -> [(S2', rest)]
    'r' : 'o' : 'l' : 'l' : ' ' : 't' : 'h' : 'r' : 'e' : 'e' : rest -> [(S3', rest)]
    'r' : 'o' : 'l' : 'l' : ' ' : 'f' : 'o' : 'u' : 'r' : rest -> [(S4', rest)]
    'r' : 'o' : 'l' : 'l' : ' ' : 'f' : 'i' : 'v' : 'e' : rest -> [(S5', rest)]
    'r' : 'o' : 'l' : 'l' : ' ' : 's' : 'i' : 'x' : rest -> [(S6', rest)]

-- (read "roll two") -> *** Exception: Prelude.read: no parse
-- (read "roll two") :: SixSidedDie' -> Two

{-
Представитель Eq:
-}
instance Eq SixSidedDie' where
  (==) S6' S6' = True
  (==) S5' S5' = True
  (==) S4' S4' = True
  (==) S3' S3' = True
  (==) S2' S2' = True
  (==) S1' S1' = True
  (==) _ _ = False
  (/=) x y = not (x == y) -- можно заккоментить, всё ещё будет раьотать

-- myDie == S5'
-- myDie == S6'

{-
Класс типов Ord
Упорядочивание значений.
Если для типа имеет смысл понятия: больше, меньше, сортировка, сравнение, min, max, ...
:i Ord

        тип уже должен быть представителем Eq для того что бы сделать его представителем Ord
        (т.е. классы расширяемы (class extantion), а интерфейсы могут наследоваться)
        Ord наследует Eq
        🠗
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a
  {-# MINIMAL compare | (<=) #-}

Законы для класса типов Ord (контроль выполнения законов на программисте):

1. x <= x ≡ True               -- Reflexivity

2. if x <= y && y <= z ≡ True,
   then x <= z ≡ True          -- Transitivity
   else x <= z ≡ False

3. if x <= y && y <= x ≡ True,
   then x == y ≡ True          -- Antisymmetry
   else x == y ≡ False

4. x <= y || y <= x ≡ True     -- Comparability

-}

instance Ord SixSidedDie' where
  compare S6' S6' = EQ
  compare S6' _ = GT
  compare _ S6' = LT
  compare S5' S5' = EQ
  compare S5' _ = GT
  compare _ S5' = LT
  compare S4' S4' = EQ
  compare S4' _ = GT
  compare _ S4' = LT
  compare S3' S3' = EQ
  compare S3' _ = GT
  compare _ S3' = LT
  compare S2' S2' = EQ
  compare S2' _ = GT
  compare _ S2' = LT
  compare S1' S1' = EQ

-- compare S1' _ = GT -- !warning!
-- compare _ S1' = LT -- !warning!
-- compare _ _ = undefined -- !warning!
-- myDie > S5'
-- myDie <= S5'

{-
Минимальное определение Ord: compare | (<=)

Через (<=) можно вывести compare:
compare x y = if x == y then EQ
                        else if x <= y then LT
                                       else GT

Через compare можно вывести все остальные функции Ord:

x < y = case compare x y of {LT -> True; _ -> False}
x <= y = case compare x y of {GT -> False; _ -> True}
x > y = case compare x y of {GT -> True; _ -> False}
x >= y = case compare x y of {LT -> False; _ -> True}
max x y = if x <= y then y else x
min x y = if x <= y then x else y
-}

{-
Класс типов Enum
Перечислимые типы. Работа с последовательностями значений

class Enum a where
  succ :: a -> a
  pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int
  enumFrom :: a -> [a]
  enumFromThen :: a -> a -> [a]
  enumFromTo :: a -> a -> [a]
  enumFromThenTo :: a -> a -> a -> [a]
  {-# MINIMAL toEnum, fromEnum #-}
-}

instance Enum SixSidedDie' where
  toEnum 0 = S1'
  toEnum 1 = S2'
  toEnum 2 = S3'
  toEnum 3 = S4'
  toEnum 4 = S5'
  toEnum 5 = S6'
  fromEnum S1' = 0
  fromEnum S2' = 1
  fromEnum S3' = 2
  fromEnum S4' = 3
  fromEnum S5' = 4
  fromEnum S6' = 5

-- succ S1'
-- pred S5'
--
-- ghci> succ S1
-- S2
-- ghci> pred S5
-- S4

{-
Класс типов Bounded
Минимальное и максимальное значение типов

class Bounded a where
  minBound :: a // минимум снизу
  maxBound :: a // максимум сверх
  {-# MINIMAL minBound, maxBound #-}

-- :i Int
-- :i Integer

-}
instance Bounded SixSidedDie' where
  minBound = S1'
  maxBound = S6'

{-
Для простых типов можно использовать механизм deriving, который попробует вывести типы самостоятельно
Это хорошо работает для простых типов и при прототипировании

Например, все что написано выше реализуется:
-}
data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Show, Read, Eq, Ord, Enum, Bounded)

myAnotherDie = S3

-- myAnotherDie // S3
-- succ myAnotherDie // S4
-- myAnotherDie > S5 // False
-- sort [myAnotherDie, S5, S1] // [S1,S3,S5]
-- (read "S1") :: SixSidedDie // S1
{-
Но он работает по стандартной схеме и нужно заранее понимать как будут использоваться конструкторы данных

Например разная последовательность записи конструкторов даст разные результаты:
-}
data Test1 = AA | ZZ deriving (Eq, Ord)

data Test2 = ZZZ | AAA deriving (Eq, Ord)

-- AA < ZZ // True (конструктор AA стоит левее, чем ZZ)
-- AAA < ZZZ // False - важно, как вы обозначали конструкторы внутри, порядок слева направо
-- хаскель не делает ортировки по буквам
-- // можно что-то писать через deriving, а что-то руками
-- AA > ZZ // False
-- AAA > ZZZ //

{-
Можно использовать deriving и рукописных представителей одновременно
+ внутри экземпляра учитываются уже написанные представители
-}

-- newtype - чтобы в процессе разработки пометить нужные нам типы
newtype MyName = MyName (String, String) deriving (Show, Eq)

instance Ord MyName where
  compare (MyName (f1, l1)) (MyName (f2, l2)) = compare (l1, f1) (l2, f2)

names = [MyName ("Emil", "Cioran"), MyName ("Eugene", "Thacker"), MyName ("Friedrich", "Nietzsche")]

-- sort names

{-
Синтаксис записей также используется в представителях классов типов:
-}
data MyNameRec = MyNameRec
  { firstName :: String,
    lastName :: String
  }
  deriving (Show, Eq)

instance Ord MyNameRec where
  compare = comparing lastName

namesRec = [MyNameRec "Emil" "Cioran", MyNameRec "Eugene" "Thacker", MyNameRec "Friedrich" "Nietzsche"]

-- sort namesRec

{-
Другие полезные классы типов:

Класс типов Num
Возможность работы с числовыми типами и обобщенный код для работы с арифметическими операциями

         допустимо множественное наследование
         ↓       ↓
class (Eq a, Show a) => Num a where --
    (+), (-), (*) :: a -> a -> a
    negate :: a -> a
    abs, signum :: a -> a
    fromInteger :: Integer -> a
    ...

Ккласс типов Real
Точные вычисления и преобразование чисел в рациональные

class (Num a, Ord a) => Real a where
  toRational :: a -> Rational
  {-# MINIMAL toRational #-}

Класс типов Integral
Работа с целочисленными типами (Int, Integer)

class (Real a, Enum a) => Integral a where
    ...

Класс типов Fractional
Работa с дробными типами (Float, Double)

class Num a => Fractional a where
    ...

Класс типов Floating
Работа с математическими функциями (тригонометрия, логарифмы, ...)

class Fractional a => Floating a where
    ...

Автоматического приведения типов нет, но есть полиморфные функции

-}

--------------------------------------------------------
{-
Cabal (Common Architecture for Building Applications and Libraries) — библиотека и система сборки
- Сборка проекта из нескольких модулей (структурирование проекта)
- Управление зависимостями (подключение внешних библиотек)
- Изоляция от окружения (локально хранит нужные версии библиотек и контролирует их совместимость)
- Автоматически скачивает библиотеки из Hackage (центрального репозитория Haskell)
- Сборка проектов, запуск тестов, ...

-------------------------------
-- Создание проекта в cabal: --
-------------------------------

1. Создание директории:
mkdir myCabalProject
cd myCabalProject

2. Инициализация проекта:
cabal init
-- ответы вопросы по специфике проекта (выбрать значения по умолчанию)
-- Executable - программа с точкой входа main
-- Main.hs - точка входа
-- app - директория для проекта
-- Haskell2010 - актуальная версия языка

Итог после выполнения п.1 и 2:
myCabalProject
  |- app
  |   |- Main.hs -- исходный код
  |- CHANGELOG.md
  |- LICENSE
  |- myCabalProject.cabal -- описание пакетов, зависимостей и прочие метаданные
                             (важный файл, можно все поломать, пишите аккуратно)

----
ilyasemenov@mac-air-m3 myCabalProject % cabal init
Config file path source is default config file.
Config file not found: /Users/ilyasemenov/.config/cabal/config
Writing default configuration to /Users/ilyasemenov/.config/cabal/config
Warning: The package list for 'hackage.haskell.org' does not exist. Run 'cabal
update' to download it.
What does the package build:
1) Library
\* 2) Executable
3) Library and Executable
4) Test suite
Your choice? [default: Executable]
Please choose version of the Cabal specification to use:
1) 1.24
2) 2.0   (support for Backpack, internal sub-libs, '^>=' operator)
3) 2.2   (+ support for 'common', 'elif', redundant commas, SPDX)
4) 2.4   (+ support for '**' globbing)
\* 5) 3.0   (+ set notation for ==, common stanzas in ifs, more redundant commas, better pkgconfig-depends)
6) 3.4   (+ sublibraries in 'mixins', optional 'default-language')
7) 3.14  (+ build-type: Hooks)
Your choice? [default: 3.0]
Package name? [default: myCabalProject]
Package version? [default: 0.1.0.0]
Please choose a license:
1) BSD-2-Clause
\* 2) BSD-3-Clause
3) Apache-2.0
4) MIT
5) MPL-2.0
6) ISC
7) GPL-2.0-only
8) GPL-3.0-only
9) LGPL-2.1-only
10) LGPL-3.0-only
11) AGPL-3.0-only
12) GPL-2.0-or-later
13) GPL-3.0-or-later
14) LGPL-2.1-or-later
15) LGPL-3.0-or-later
16) AGPL-3.0-or-later
17) Other (specify)
Your choice? [default: BSD-3-Clause]
Author name? [default: Ilya Semenov]
Maintainer email? [default: hi@si1og.ru]
Project homepage URL? [optional]
Project synopsis? [optional]
Project category:
1) Codec
2) Concurrency
3) Control
4) Data
5) Database
6) Development
7) Distribution
8) Game
9) Graphics
10) Language
11) Math
12) Network
13) Sound
14) System
15) Testing
16) Text
17) Web
18) Other (specify)
Your choice? [default: (none)]
What is the main module of the executable:
\* 1) Main.hs
2) Main.lhs
3) Other (specify)
Your choice? [default: Main.hs]
Application directory:
\* 1) app
2) exe
3) src-exe
4) Other (specify)
Your choice? [default: app]
Choose a language for your executable:
\* 1) Haskell2010
2) Haskell98
3) GHC2021 (requires at least GHC 9.2)
4) GHC2024 (requires at least GHC 9.10)
5) Other (specify)
Your choice? [default: Haskell2010]
Add informative comments to each field in the cabal file. (y/n)? [default: y]
[Info] Using cabal specification: 3.0
[Info] Creating fresh file LICENSE...
[Info] Creating fresh file CHANGELOG.md...
[Info] Creating fresh directory ./app...
[Info] Creating fresh file app/Main.hs...
[Info] Creating fresh file myCabalProject.cabal...
[Warn] No synopsis given. You should edit the .cabal file and add one.
[Info] You may want to edit the .cabal file and add a Description field.
----

3. Сборка проекта (выполняется в корневом каталоге проекта)
cabal build
-- cabal скачает необходимые зависимости из Hackage, скомпилирует их и проект (все новое закэшируется в папке)
-- ! появится новая директория dist-newstyle

----
ilyasemenov@mac-air-m3 myCabalProject % cabal build
Warning: The package list for 'hackage.haskell.org' does not exist. Run 'cabal
update' to download it.
Resolving dependencies...
Build profile: -w ghc-9.6.7 -O1
In order, the following will be built (use -v for more details):
 - myCabalProject-0.1.0.0 (exe:myCabalProject) (first run)
Configuring executable 'myCabalProject' for myCabalProject-0.1.0.0...
Preprocessing executable 'myCabalProject' for myCabalProject-0.1.0.0...
Building executable 'myCabalProject' for myCabalProject-0.1.0.0...
[1 of 1] Compiling Main             ( app/Main.hs, dist-newstyle/build/aarch64-osx/ghc-9.6.7/myCabalProject-0.1.0.0/x/myCabalProject/build/myCabalProject/myCabalProject-tmp/Main.o )
[2 of 2] Linking dist-newstyle/build/aarch64-osx/ghc-9.6.7/myCabalProject-0.1.0.0/x/myCabalProject/build/myCabalProject/myCabalProject
----

4. Запуск проекта
cabal run
-- автоматически выполнит build если есть изменения
-- На выходе получим результаты работы main: Hello, Haskell!

----
ilyasemenov@mac-air-m3 myCabalProject % cabal run
Hello, Haskell!
-----

~. Запуск интерактивного режима для удобства разработки (Read-Eval-Print Loop)
cabal repl
-- ! загрузится ghci, но в этом случае она будет содержать в себе все функции, библиотеки, модули и пр. которые есть в проекте с учетом доступа и видимости из main

----
ilyasemenov@mac-air-m3 myCabalProject % git add .gitignore
fatal: pathspec '.gitignore' did not match any files
ilyasemenov@mac-air-m3 myCabalProject % cabal repl
Build profile: -w ghc-9.6.7 -O1
In order, the following will be built (use -v for more details):
 - myCabalProject-0.1.0.0 (interactive) (exe:myCabalProject) (configuration changed)
Configuring executable 'myCabalProject' for myCabalProject-0.1.0.0...
Preprocessing executable 'myCabalProject' for myCabalProject-0.1.0.0...
GHCi, version 9.6.7: https://www.haskell.org/ghc/  :? for help
[1 of 2] Compiling Main             ( app/Main.hs, interpreted )
Ok, one module loaded.
ghci>
----

-------------------------------
Добавление зависимостей в проект:

Заходим в файл _.cabal (в нашем случае myCabalProject.cabal)
    Ищем
          executable _ (в нашем случае executable myCabalProject)
    Ищем поле
              build-depends:    base ...
    Записываем под base дополнительные пакеты для скачивания например text (версии можно указывать гибко (>=), фиксировано (==) и в диапазоне (>=1.2 && <1.3))
                                text >=1.2.5
Импортируем библиотеки в модуль (в нашем случае Main)
  import qualified Data.Text as T
Пере собираем проект (cabal build) и cabal автоматически проанализирует и подгрузит новые нужные библиотеки

Добавим в проект модуль MyModule и импортируем его в Main и настроим ограничения доступа к функциям
Импортировать всё и везде не нужно, ограничивайтесь только теми участками кода, в которых используются функции

Если проект уже содержит в себе все зависимости (они кэшированы) можно собирать проект оффлайн:
cabal build --offline
cabal run --offline

Для старых проектов полезно замораживать версии пакетов (т.к. скачивание новых версий может сломать программу):
cabal freeze
-- cabal freeze создаст новый файл (cabal.project.freeze) с точными версиями пакетов и при следующих сборках cabal будет придерживаться точных версий пакетов
-------------------------------

-}
