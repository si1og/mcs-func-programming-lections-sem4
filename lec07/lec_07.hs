-----------------------------------------------------------------------------------
-- Лекция 07 Аппликативные парсеры. Parsec. Attoparsec. Alternative. Traversable --
-----------------------------------------------------------------------------------

module Lec07 where

import Data.Char (isLower, isUpper, digitToInt, isDigit)
import Control.Applicative (Alternative(..), optional, ZipList(..))

--------------------------------------------------------
{-
Синтаксический анализатор (парсер) - программа, которая преобразует входные данные в структурированный формат 
                                     для последующего использования по назначению или последующего анализа, 
                                     если входные данные не удовлетворяют грамматике, то возвращается содержательная ошибка

Примеры парсеров:
    Трансляторы (компиляторы, интерпретаторы) - текст -> команды
    Разбор баз данных (CSV, XML, ...)
    Разбор фалов 3D объектов
    Анализ текстов
    ...



Нужно одну структуру превратить в другую по заранее определенным правилам


Возможные парсеры (пусть можем анализировать сложение и цифры):

1. Получаем строку и превращаем его в готовую структуру
Например: 
    "1" :: String   →   (1 :: Int)

type Parser a = String -> a
                Строка -> Структура


2. Берем строку и разбираем часть строки, а оставшуюся строку передаем дальше
Например:
    "+ 1 2" :: String    →   (("1 2" :: String), (+ :: Int -> Int -> Int))

type Parser a = String -> (String, a)
    Комбинирование парсеров: (Остаток строки, Структура)


3. Берем строку и разбираем часть строки, а оставшуюся строку передаем дальше, возвращаем Nothing в случае ошибки
Например: 
    "- 1 2" :: String    →   Nothing

type Parser a = String -> Maybe (String, a)
    Обработка простых ошибок


4. Берем строку и разбираем часть строки, а оставшуюся строку передаем дальше, в случае ошибки будем использовать другой парсер
Например: 
    "- 1 2" :: String    →   Left ... используем другой парсер ...

type Parser a = String -> Either String (String, a)
    Обработка сложных ошибок


5. Берем строку и пытаемся распарсить ее несколькими способами (эффект множественных результатов)

type Parser a = String -> [(String, a)]
    Обработка неоднозначных грамматик



Напишем синтаксический анализатор, который может обрабатывать простые ошибки и обрабатывать лексемы по типу входного потока
Тип парсера:
    type Parser tok a = [tok] -> Maybe ([tok], a)



newtype                                                                 -- ключевое слово для объявления нового типа
        Parser                                                          -- имя определяемого парсера (типа)
               tok                                                      -- параметр типа (тип входного потока String, Char, Int, ...)
                   a                                                    -- параметр типа (тип результата Int, Double, ...)
                               runParser                                -- функция запускающая парсер (поле записи)
                                            [tok]                       -- входной тип парсера (список токенов)
                                                     Maybe ([tok], a)   -- результат работы парсера (Nothing - неудача, Just _ - успех)
                                                           ([tok], a)   -- тип успешного парсинга
                                                            [tok]       -- оставшиеся неразобранные токены
                                                                   a    -- результат парсинга
newtype Parser tok a = Parser {runParser :: [tok] -> Maybe ([tok], a) }
-}
newtype Parser tok a = Parser {runParser :: [tok] -> Maybe ([tok], a) }

{-
Простейший парсер, который проверяет наличие буквы 'A' в начале строки


Как написать простой парсер?

parserName :: ParserType InputStreamType ResultStructureType
parserName = ParserConstructor function where
    function processing


В случае с парсингом буквы:

              Parser     tok          a
                🠗         🠗           🠗
имяПарсера :: Парсер ПринимаетЧар СтруктураЧар

                    Parser     [tok] -> Maybe ([tok], a)
                      🠗              🠗
имяПарсера = КонструкторПарсера   стрелка where -- парсер - это конструктор парсера, содержащий в себе стрелку (функцию), которая
    стрелка (разбиваем входной поток на голову и хвост) если голова потока эот буква 'A' то собираем структуру возможного значения с парой из неразобранной части строки и буквой 'A'
    стрелка в противном случае является неудачным парсингом и возвращаем отсутствующее значение
-}

-- charA :: ...
-- charA = ...

{-
Получили простой парсер, который может определить является ли первый символ символом 'A' или нет


Как запустить такой парсер?

Нужно вынуть стрелку (функцию) из упаковки парсера и передать в нее поток для синтаксического анализа

Запуск парсера:
runParser parserName InputStream


Например:

runParser charA "ABC"

Как работает запуск парсера на уровне типа и реализации:
Запуск парсера через функцию runParser
    В runParser передается парсер charA
        runParser вынимает из charA стрелку: [tok] -> Maybe ([tok], a)
        т.е. возвращается функция из списка токенов [tok] в Maybe ([tok], a)

    В полученную функцию передается строка "ABC"
    Выполняется: f (c:cs) | c == 'A' = Just (cs,c)
        здесь удачное сопоставление с образцом и удачный результат охранного выражения
        т.к. первый символ 'A' то возвращается сборка для Maybe ([tok], a)
        т.е. Just ("BC",'A')

-- runParser charA "BCD"
Запуск -//-
    Выполняется: f (c:cs) | c == 'A' = Just (cs,c)
        здесь удачное сопоставление с образцом и НЕ удачный результат охранного выражения
        т.к. первый символ 'B' переходим к следующей строке
    Выполняется: f _ = Nothing, возвращается Nothing





Парсер charA ограничен в функционале.
Нужно его расширить так что бы можно было использовать любой предикат для первого символа
Т.е. функция принимает предикат и возвращает парсер

По аналогии с charA используем унарный предикат в охранных выражениях и сохраним оставшийся код без изменений
Предикаты работаю не только с Char, так что можно использовать полиморфный тип
-}

-- satisfy :: ...
-- satisfy ...

{-
Унарные предикаты из Data.Char для парсинга символов:
    isLower
    isUpper
    isDigit


Сборка и запуск парсеров с предикатами:
runParser (satisfy isUpper) "ABC"
runParser (satisfy isLower) "ABC"
runParser (satisfy isLower) "abc"
runParser (satisfy isDigit) "ABC"



Можно обобщить парсер для анализа любой буквы по выбору:
-}
--char :: ...
--char ...
-- runParser (char 'a') "asd"

--lower ::...
--lower = ...
-- runParser lower "asd"



{-
Получившийся парсер и набор предикатов реализуют возможность работать с текстом посимвольно


Напишем парсер для реализации вычислений простых математических операций
Например: вычислить выражение в строке: "12*345" и получить результат типа Int


Используем функцию isDigit из Data.Char для парсинга цифр:

runParser (satisfy isDigit) "1BC"
> Just ("BC",'1')

Цифра получилась, но она типа Char
Автоматического приведения типов в Haskell нет и Char не поддерживаем математических операций

В Data.Char есть функция 
:i digitToInt
Она приводит символ к нужному нам типу



Итого на данный момент есть:
1. парсер который может разбирать первый символ в потоке Char
2. функция, которая может переводить Char -> Int
Другими словами:
1. Значение в контексте (контекст разбора строк)
2. Чистая функция

Объединить две эти части и получить результат можно через функтор
(это исходит из типов)

                           Parser Char Int
                           🠗
fmap :: (a -> b) -> f a -> f b
            🠕       🠕
            🠕       Parser Char Char
        digitToInt :: Char -> Int


Теперь нужно сделать Parser представителем Functor:
Для этого вынимаем и разбираем стрелку из входного парсера и применяем функцию к аргументу затем пересобираем выходной парсер
-}

--instance Functor (Parser tok) where
--    fmap :: (a -> b) -> Parser tok a -> Parser tok b
--    fmap ...

{-

Parser - это последовательная композиция трех функторов:
    (->) [tok], Maybe и (,) [tok]
Значит можно реализовать представителя через fmap и переупаковку:
    ...
    или
    ...


Соберем парсер для цифр с использованием функторов:
-}
--digit :: Parser Char Int
--digit = ...

{-
runParser digit "12AB"
runParser digit "AB12"

Получившийся парсер решает задачу определения цифры в первом символе строки и приведения ее к типу Int
-}





--------------------------------------------------------
{-
Сцепление парсеров

Для обработки выражений типа "12+345" нужно уметь продвигаться по строке вправо и парсить каждый следующий символ
Для этого нужно уметь сцеплять парсеры

Два сцепленных парсера - это значения и функции в контексте
Т.е. нужно сделать Parser представителем Applicative
Сцепление нескольких парсеров в один общий парсер производит несколько эффектов, которые реализуют синтаксический разбор

Логика Applicative для парсеров:
    pure: парсер, всегда возвращающий заданное значение
    (<*>): получить результаты первого парсера, затем второго, а после этого применить первые ко вторым

-}

--instance Applicative (Parser tok) where
{-
без эффектное вычисление - строка не обрабатывается и вместе со значением кладется в упаковку
В конструктор парсера упаковывается стрелка (написанная, например через лямбду)
       входом, который является списком токенов, а выходом структура типа Maybe и пара первым элементом которой является список токенов, а вторым - значение которое хотим поместить в контекст
runParser (pure 42) "ABCD"
-}
--    pure :: a -> Parser tok a
--    pure x = ...

{-
В конструктор парсера упаковывается стрелка, которая
    обрабатывает стрелку из первого парсера и сохраняет эффект отсутствующего значения или 
        запускает второй парсер на результате работы первого и также сохраняет эффект или применяет функцию из первого парсера к значению из второго
Неудача одного из парсеров вернет неудачу по всему парсингу

-- runParser (pure (,) <*> digit <*> digit) "12AB"
-- runParser ((,) <$> digit <*> digit) "1AB2"
-- runParser ((,) <$> digit <*> charA) "1AB2"
-}
--    (<*>) :: Parser tok (a -> b) -> Parser tok a -> Parser tok b
--    Parser u (<*>) Parser v = ...


{-
Соберем простой парсер на основе аппликатива для разбора строки типа: "2*3"
-}
-- multiplication :: ...
-- multiplication = ...
{-
runParser multiplication "2*3"
runParser multiplication "g*3"
runParser multiplication "12*345"

Парсинг "12*345" через <*> требует только удачных сопоставлений

Это почти нужный результат
Осталось получить следующее поведение:
    парсить цифры пока не наткнулись на знак,
    прочитать знак,
    дальше парсить цифры до конца
Т.е. нужно иметь несколько парсеров, которые можно применять к одному токену из потока
Если первый парсер не сработал, то использовать альтернативный и т.д. пока не кончатся парсеры или пока не получим успех
-}





--------------------------------------------------------
{-
Класс типов Alternative

class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a
    some :: f a -> f [a]
    many :: f a -> f [a]
    {-# MINIMAL empty, (<|>) #-}

Методы empty и (<|>) образуют моноидальную операцию с семантикой сложения



Представитель для списка [] - полный аналог моноида для списка
instance Alternative [] where
    empty :: [a]
    empty = []
    (<|>) :: [a] -> [a] -> [a]
    (<|>) = (++)



Представитель моноида для Maybe:
class Monoid a where
    mempty :: a
    mappend :: a -> a -> a
instance Monoid a => Monoid (Maybe a) where -- ограничение моноида на содержимое контейнера
    mempty = Nothing
    Nothing `mappend` m = m
    m `mappend` Nothing = m
    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2) -- содержание контейнера тоже должно быть моноидом

Представитель Alternative для Maybe не повторяет моноид:
instance Alternative Maybe where
    empty :: Maybe a
    empty = Nothing

    (<|>) :: Maybe a -> Maybe a -> Maybe a
    Nothing <|> m = m
    m <|> _ = m -- возвращаем первую не Nothing альтернативу

Nothing <|> Just 3 <|> Just 5 <|> Nothing



Alternative для списков с zip семантикой:
instance Alternative ZipList where
    empty :: Maybe a
    empty = ZipList []

    (<|>) :: ZipList a -> ZipList a -> ZipList a
    ZipList xs <|> ZipList ys = ZipList (xs ++ drop (length xs) ys) -- дополняем первый список остатками второго

ZipList "abc" <|> ZipList "ABCDEFG"





Сделаем парсер представителем класса типов Alternative:
-}

--instance Alternative (Parser tok) where
{-
Пустой парсер не должен мешать успешным вычислениями
Поэтому метод empty игнорирует входной поток и убивает вычисление отсутствующим значением
-}
--    empty :: Parser tok a
--    empty = ...

{-
Если результат первого парсера валидный, то возвращаем его, в противном случае возвращаем второй парсер
-}
--    (<|>) :: Parser tok a -> Parser tok a -> Parser tok a
--    Parser u <|> Parser v = ...


-- runParser  (char 'A' <|> char 'B') "ABC"
-- runParser  (char 'A' <|> char 'B') "BCD"
-- runParser  (char 'A' <|> char 'B') "CDE"
-- runParser  (empty <|> char 'A' <|> char 'B') "BCF"



{-
Напишем персер определяющий все символы в нижнем регистре в начале строки (т.е. парсим троку пока есть символы в нижнем регистре)

Используем 
- парсер lower, написанный выше
- альтернативу (<|>)

Такой рекурсивный парсер из конструктора строк, парсера нижних символов, рекурсивного вызова, терминирующего парсера без эффектов
-}
--lowers :: Parser Char String
--lowers = ...

-- runParser lowers "abGHdef"
-- runParser lowers "abdef"
-- runParser lowers "GHabdef"



{-
Парсеры типа lowers обобщаются

Методы some, many, optinal

class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a

    some, many :: f a -> f [a]
    some v = (:) <$> v <*> many v -- Один и более
    many v = some v <|> pure [] -- Ноль и более

    optional :: Alternative f => f a -> f (Maybe a)
    optional v = Just <$> v <|> pure Nothing



Если исход успешный то результат одинаковый:
runParser (many digit) "42abdef"
runParser (some digit) "42abdef"

Если парсинг не успешен, ...:
runParser (many digit) "abdef" -- вычисление можно продолжить не разобранная строка сохраняется
runParser (some digit) "abdef" -- вычисление убивается полностью

Строку можно сохранять, но создавать эффект отсутствующих значений в результирующем типе:
runParser (optional digit) "42abdef"
runParser (optional digit) "abdef"





Используем все методы для получения парсера для выражения типа "12*345"
-}
-- ...

-- finalMult :: Parser Char Int
-- finalMult = ...

{-
runParser finalMult "12*345"
runParser finalMult "12*345dsf"


Что если я хочу не умножать, а складывать?
-}
--finalPlus :: Parser Char Int
--finalPlus = ...
{-
runParser finalPlus "12+345dsf"


Что если я не хочу думать умножаю я или складываю?
-}
--plusOrMult :: 
--plusOrMult = 
{-
runParser plusOrMult "12*345dsf"
runParser plusOrMult "12+345dsf"
-}





--------------------------------------------------------
{-

Parsec - библиотека для создания парсеров
Конструирование сложных парсеров из множества маленьких

- Использует концепцию парсеры как значения - абстракции высокого уровня. Т.е. парсеры можно передавать и возвращать как обычные значения
- Комбинаторы - функции, которые берут парсеры и соединяют их в более сложные парсеры
- Монадический подход - возможность использовать do нотацию





Attoparsec - библиотека для создания высокопроизводительных парсеров (максимальная скорость работы)

- Специализированные парсеры для работы с целыми участками текста, а не отдельным символам
- Работа с инкрементальным входом. Можно распарсить только часть данных и это не приведет к ошибке, будет результат частичного парсинга
- Различные типы строк, в том числе строгие Text и ByteString

-}





--------------------------------------------------------
{-
Есть случаи, в которых наличие строгой структуры обязательно
Например, структура даты, ключевые слова, кортежи, записи, ...

parIF :: [Parser Char Char]
parIF = [char 'i', char 'f']

parTHEN :: [Parser Char Char]
parTHEN = [char 't', char 'h', char 'e', char 'n']

parELSE :: [Parser Char Char]
parELSE = [char 'e', char 'l', char 's', char 'e']

sequenceParIF :: Parser Char [Char]
sequenceParIF = sequenceA parIF
-}
-- runParser sequenceParIF "if123asd"

{-
Из примера видно что список парсеров можно превратить в парсер со списком результатов
                   [Parser Char Char]                             Parser Char [Char]
Т.е. это аппликативный дистрибьютор списка


Как это работает на уровне типов и каиндов?
[f a] -> f [a]
    f :: * -> *
    [] :: * -> *
Изменение порядка контейнеров

Реализация функции дистрибьютора списка для произвольного аппликатива:
-}

--                  контекст аппликатива
--                  🠗     🠗       🠗
dist :: Applicative f => [f a] -> f [a]
-- пустой список нужно просто упаковать в контейнер аппликатива без эффектов
dist [] = pure []
-- рекурсивный разбор списка и изменение порядка контейнеров
dist (ax:axs) = pure (:) <*> ax <*> dist axs
--              🠕     🠕
--              🠕     пере сборка списка
--              упаковка от аппликатива со списком внутри
-- все эффекты сохраняются, но меняются местами
{-
Изменение порядка эффектов списка и Maybe:
dist [Just 3,Just 5]
dist [Just 3,Nothing]

У списков две реализации с двумя эффектами (пары и декартово произведение):
getZipList $ dist $ map ZipList [[1,2], [3,4], [5,6]]
dist [[1,2], [3,4], [5,6]]
-}



{-
Класс типов Traversable

Можно обобщить список до произвольного контейнера t :: * -> *
f :: * -> *
t :: * -> *
t (f a) -> f (t a)

class (Functor t, Foldable t) => Traversable t where
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    sequenceA :: Applicative f => t (f a) -> f (t a)
    mapM :: Monad m => (a -> m b) -> t a -> m (t b)
    sequence :: Monad m => t (m a) -> m (t a)
    {-# MINIMAL traverse | sequenceA #-}



sequenceA - определяет правило коммутации функтора t с произвольным аппликативным функтором f
структура внешнего контейнера t сохраняется, а аппликативные эффекты внутренних f объединяются в результирующем f

sequenceA :: Applicative f => t (f a) -> f (t a)
sequenceA = traverse id
sequenceA = traverse    :: (a -> f b) -> t a -> f (t b)
                     id :: (a -> f b)
                            a = f b
                     id :: f b -> f b


sequenceA (Just [1,2,3])
sequenceA [Just 1, Just 2, Just 3]
sequenceA [Just 1, Nothing, Just 3]



traverse в качестве входного значения требует функцию которая в явном виде приводит чистые значения из контейнера t и поднимает их в контекст f
поднимает стрелку (a -> f b) в вычислительный контекст
проходим по структуре t a, последовательно применяя функцию к элементам типа a и собирая структуру из результатов типа b
эффекты коллекционируются (fmap с эффектами)

traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
traverse g = sequenceA . fmap g
                         fmap g ::         t a -> f (t b)
             sequenceA                         -> f (t b)

traverse (\x -> [x+10, x+200]) (Just 7)
traverse (\x -> [x+10, x+200]) [7,8,9]
traverse (\x -> [x+10, x+200, x+3000]) [7,8]

traverse :: (a  -> f b)           -> t a     -> f (t b)
traverse    (\x -> [x+10, x+200])    [7,8,9]
                                     t - трехэлементный список - внутренняя структура
                   f - двухэлементный список - реализует семантику аппликатива
                                                f  t



Представители базовых типов для класса типов Traversable:

instance Traversable Maybe where
    traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
    traverse _ Nothing = pure Nothing
    traverse g (Just x) = Just <$> g x

instance Traversable [] where
    traverse :: Applicative f => (a -> f b) -> [a] -> f [b]
    traverse _ [] = pure []
    traverse g (x:xs) = (:) <$> g x <*> traverse g xs

Представители Traversable аналогичны по структуре представителям Functor



Сравнение реализаций Traversable и Functor

instance Traversable Maybe where
    traverse _ Nothing  = pure Nothing
    traverse g (Just x) = pure Just <*> g x
instance Functor Maybe where
    fmap     _ Nothing  =      Nothing
    fmap     g (Just x) =      Just    (g x)

instance Traversable [] where
    traverse _ []     = pure []
    traverse g (x:xs) = pure (:) <*> g x <*> traverse g xs
instance Functor [] where
    fmap _ []         =      []
    fmap g (x:xs)     =      (:)    (g x)   (fmap     g xs)


Traversable — это Functor: имея traverse мы можем универсальным образом реализовать fmap, удовлетворяющий законам функтора
Можно реализовать содержательно Traversable, а Functor и Foldable получить реализациями по умолчанию





Законы Traversable (наследуют законам функторов):

(1) identity:
traverse Identity ≡ Identity

newtype Identity a = Identity { runIdentity :: a } deriving (Show) -- Id :: a -> Id a
instance Functor Identity where
    fmap g (Identity x) = Identity (g x)
instance Applicative Identity where
    pure = Identity
    Identity g <*> v = fmap g v

traverse Identity [1,2,3]
runIdentity (traverse Identity [1,2,3])
runIdentity (traverse Identity (Just 1))


(2) composition
traverse (Compose . fmap f2 . f1) ≡ Compose . fmap (traverse f2) . traverse f1
                              f1 :: a -> g1 b
                         f2 :: b -> g2 c
    обе части :: t a -> Compose g2 g1 (t с)
Композиция (a -> g1 b) и (b -> g2 c) эквивалентна (a -> g1 (g2 c))


(3) naturality
h . traverse f ≡ traverse (h . f)

h :: (Applicative f, Applicative g) => f b -> g b - функция удовлетворяющая требованиям:
    (1) h (pure x) = pure x
    (2) h (x <*> y) = h x <*> h y
f :: a -> f b, обе части :: t a -> g (t b)



Traversable не пропускает элементы про проходе по структуре,
    посещает элементы не более одного раза,
    не изменяет исходную структуру и в итоге она либо сохраняется, либо полностью исчезает

traverse Just [1,2,3]
traverse (const Nothing) [1,2,3]
-}





--------------------------------------------------------
{-
import Data.Monoid (Alt(..),Ap(..),Sum(..))



Некоторые типы являются Alternative, но не являются Monoid
Например:
    Maybe a - Alternative, но не Monoid для произвольного a
    [] a (списки) - Alternative, но не Monoid для произвольного a

Обертка Alt
Alt превращает любой Alternative функтор в Semigroup и Monoid, используя операцию <|> из Alternative

newtype Alt f a = Alt {getAlt :: f a}
instance Alternative f => Semigroup (Alt f a) where
    (<>) = coerce ((<|>) :: f a -> f a -> f a)
    stimes = stimesMonoid
instance Alternative f => Monoid (Alt f a) where
    mempty = Alt empty

-- Alt "Abc" <> Alt "De"
-- Alt Nothing <> Alt (Just 1) <> Alt (Just 2)

Alt - использует моноидные функции (mconcat, foldMap и т.д.) с Alternative типами





Если у нас есть Applicative f и Monoid a, то (f a) обычно не является моноидом автоматически

Обертка Ap
Ap позволяет "поднять" операцию моноида (<>, mempty) внутрь аппликативного функтора (Applicative f), используя liftA2.

newtype Ap f a = Ap { getAp :: f a }
instance (Applicative f,Semigroup a) => Semigroup (Ap f a) where
    (Ap x) <> (Ap y) = Ap $ liftA2 (<>) x y
instance (Applicative f, Monoid a) => Monoid (Ap f a) where
    mempty = Ap $ pure mempty

-- apLstS1 = Ap $ Sum <$> [1,2,3]
-- apLstS2 = Ap $ Sum <$> [10,20]
-- getAp $ getSum <$> (apLstS1 <> apLstS2)

Ap - комбинирует эффекты аппликативных функторов с сохранением структуры моноида
-}
