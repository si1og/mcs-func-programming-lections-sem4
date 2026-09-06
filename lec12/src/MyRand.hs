module MyRand where

import System.Random (mkStdGen) -- dependencies: - random
import System.Random.Stateful
import Data.List (sort)
import Data.Char (isAscii, isControl, isLetter)

--------------------------------------------------------
{-
Напишем функцию, создающую псевдослучайные значения. Для этого создаем
    - тип, который хранит состояние
    - функцию, которая по случайному состоянию создает случайное число и новое случайное состояние
-}
newtype RandomState = RandomState Int deriving (Eq, Show)

randomInt :: RandomState -> (Int, RandomState)
randomInt (RandomState rs) = (newRs, RandomState newRs)
    where newRs = (1103515245 * rs + 12345) `mod` (2 ^ 31)
{-
randomInt (RandomState 100)

Расширим функцию для генерации списка слученых значений:
-}
randomIntList :: RandomState -> Int -> [Int]
randomIntList rs n
    | n <= 0 = []
    | otherwise =
        let (v, rs') = randomInt rs
        in v : randomIntList rs' (n - 1)
{-
randomIntList (RandomState 100) 5


Написанные функции не универсальны
Случайные значения могут быть разных типов


Для получения значений разных типов можно использовать StdGen из System.Random (или более современный модуль System.Random.Stateful)


RandomState это формально StdGen
Создание StdGen с начальным числом (seed = 100):
g = mkStdGen 100
g
random генерирует кортеж из случайного числа и модифицированного StdGen
random g :: (Int, StdGen)


Стандартный набор функций генерации случайных значений:
random генерирует случайные значения, где распределение возможных значений типа не известно
uniform генерирует равномерно распределенные случайные значения для данного типа
обе функции с суффиксом R, генерируют случайные значения в заданном диапазоне

random :: (Random a, RandomGen g) => g -> (a, g)
randomR :: (Random a, RandomGen g) => (a, a) -> g -> (a, g)
uniform :: (Uniform a, RandomGen g) => g -> (a, g)
uniformR :: (UniformRange a, RandomGen g) => (a, a) -> g -> (a, g)

Эти функции позволяют написать поиморфный код:
-}

randomListN :: (Random a) => StdGen -> Int -> ([a], StdGen)
randomListN gen n
    | n <= 0 = ([], gen)
    | otherwise =
        let (v, gen') = random gen
            (xs, gen'') = randomListN gen' (n - 1)
        in (v : xs, gen'')
--randomListN (mkStdGen 100) 10

-- случайный список случайного размера с модифицированным генератором
randomList :: (Random a) => StdGen -> Int -> ([a], StdGen)
randomList gen maxVal = randomListN gen' n
    where (n, gen') = uniformR (0, maxVal) gen
--randomList (mkStdGen 100) 10

-- случайный список размером от 0 до 100
randomList' :: (Random a) => StdGen -> ([a], StdGen)
randomList' = flip randomList 100
--randomList' (mkStdGen 100)



{-
Наисанные выше требуют явного указания сида на основе которого будет произведен рассчет псевдослучайных знаений
Но System.Random предоставляет два IO действия:
    getStdGen :: IO StdGen
    setStdGen :: StdGen -> IO ()
Они дают доступ к глобальному генератору случайных состояний
Глобальный генератор (global StdGen) преинициализирован в начале программы

g1 <- getStdGen
g1
random g1 :: (Int, StdGen)
getStdGen
getStdGen возвращает не ссылку на глобальный генератор, а его копию
поэтому модифицированный генератор нужно записать обратно в глобальное состояние
--------------------------------------------------------
g1 <- getStdGen
let (num1, g1) = random g1 :: (Int, StdGen)
setStdGen g1
g1
g1' <- getStdGen
let (num2, g2) = random g1' :: (Int, StdGen)
g1'
getStdGen
-}

randomListIO :: (Random a) => IO [a]
randomListIO = do
    g <- getStdGen -- извлечение глобального генератора
    let (xs, g') = randomList' g -- генерация списка и модификация StdGen
    setStdGen g' -- сохранение модифицированного StdGen как глобальный генератор 
    return xs -- возвращение списка
{-
randomListIO :: IO [Bool]
randomListIO :: IO [Int]
randomListIO :: IO [Int]

реализация randomListIO не является безопастной в конкурентном и параллельном программировании

Для безопасных реализаций используются атомарные генераторы
:t globalStdGen
:i AtomicGenM
-}
applyGlobalStdGen :: (StdGen -> (a, StdGen)) -> IO a
applyGlobalStdGen f = applyAtomicGen f globalStdGen
{-
applyAtomicGen random globalStdGen :: IO Int
applyAtomicGen (uniformR (0, 100)) globalStdGen :: IO Float
applyAtomicGen randomList' globalStdGen :: IO [Bool]
-}



--------------------------------------------------------
{-
Используем генераторы случайных значений для проверки корректности работы функций
(разработаем базу фреймворка для тестирования свойств)


проверка отсортированности списка по возрастанию

функция проверяет каждый элемент списка меньше последующего:
-}
sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [_] = True
sorted (x : y : ys) = x <= y && sorted (y : ys)
-- проверка свойства отсортированности для функции и списка:
sorts :: Ord a => ([a] -> [a]) -> [a] -> Bool
sorts f input = sorted $ f input
{-
sort `sorts` [5,4,3,1,2 :: Int]
sort `sorts` ([] :: [Int])
id `sorts` [5,4,3,1,2 :: Int]


Тестирование свойств функции сортировки
-}
propertyTestSorts :: ([Int] -> [Int]) -> Int -> IO ()
propertyTestSorts f n | n <= 0 = putStrLn "Test successful!"
                      | otherwise = do
                            xs <- applyGlobalStdGen randomList' -- хардкод генераторов случайных списков
                            if f `sorts` xs then propertyTestSorts f $ n - 1
                               --   🠕
                               -- проверяется только свойство отсортированности
                                            else putStrLn $ "Test failed on: " <> show xs

{-
Тестирование свойств функции с полиморфными входами/выходами
-}
propertyTest :: Show a => (a -> b) -> (b -> Bool) -> IO a -> Int -> IO ()
propertyTest fun predicate random n | n <= 0 = putStrLn "Test successful!"
                                    | otherwise = do
                                        testCase <- random -- можем передавать все что угодно в качестве генераторов и списков
                                        if predicate $ fun testCase then propertyTest fun predicate random $ n - 1
                                        --     🠕
                                        -- произвольный предикат
                                                                    else putStrLn $ "Test failed on: " <> show testCase

{-
propertyTest sort sorted (applyGlobalStdGen randomList' :: IO [Int]) 100
propertyTest id sorted (applyGlobalStdGen randomList' :: IO [Int]) 100



Генерация случайных значений с условиями в IO действиях
    композиция IO для получения полиморфных значений в том числе списков
-}
newtype RandomIO a = RandomIO { runRandomIO :: IO a }

one :: Random a => RandomIO a
one = RandomIO $ applyGlobalStdGen random

some :: Random a => RandomIO [a]
some = RandomIO $ do
    n <- applyGlobalStdGen $ uniformR (0, 100)
    replicateIO n $ runRandomIO one

replicateIO :: Int -> IO a -> IO [a]
replicateIO n act | n <= 0 = return []
                  | otherwise = do
                        x <- act
                        xs <- replicateIO (n - 1) act
                        return $ x : xs
{-
runRandomIO one :: IO Float
runRandomIO some :: IO [Bool]


Фильтрация нежелательных значений
-}
suchThat :: RandomIO a -> (a -> Bool) -> RandomIO a
suchThat rand pred = RandomIO $ do
    val <- runRandomIO rand
    if pred val then return val -- удовлетворяет предикату
                else runRandomIO $ suchThat rand pred -- генерируется новое значение

nonNegative :: (Num a, Ord a, Random a) => RandomIO a
nonNegative = one `suchThat` (> 0)

nonEmpty :: Random a => RandomIO [a]
nonEmpty = some `suchThat` (not . null)

{-
сгенерируем список Char который можно было бы вывести на экран и его удобно было бы читать человеку
    используем 
        some - случайный список значений
        suchThat - фильтрация значений
        isAscii и isControl - предикаты для нужных символов
-}
asciiString :: RandomIO [Char]
asciiString = some `suchThat` all (\c -> isAscii c && not (isControl c))
{-
runRandomIO asciiString

почему такой результат?
    один неправильный символ приводит к полностью новой генерации


генерируем один символ
-}
asciiChar :: RandomIO Char
asciiChar = one `suchThat` (\c -> isAscii c && not (isControl c))
-- runRandomIO asciiChar
letterChar :: RandomIO Char
letterChar = asciiChar `suchThat` isLetter
-- runRandomIO letterChar

manyOf :: RandomIO a -> RandomIO [a]
manyOf rio = RandomIO $ do
    n <- applyGlobalStdGen $ uniformR (0, 100)
    replicateIO n (runRandomIO rio)

asciiStringUpdate :: RandomIO String
asciiStringUpdate = manyOf asciiChar
-- runRandomIO asciiStringUpdate
letterString :: RandomIO String
letterString = manyOf letterChar
-- runRandomIO letterString

{-
Теперь модифицируем propertyTest так что бы она использовала
    RandomIO как генератор
    один предикат для определения корректности теста

propertyTest :: Show a => (a -> b) -> (b -> Bool) -> IO a -> Int -> IO ()
-}
propertyTestUpdate :: Show a => (a -> Bool) -> RandomIO a -> Int -> IO ()
{-                                  🠕
            проверка свойств зашивается напрямую в предикат -}
propertyTestUpdate predicate random n | n <= 0 = putStrLn "Test successful!"
                                      | otherwise = do
                                            testCase <- runRandomIO random
                                            if predicate testCase then propertyTestUpdate predicate random $ n - 1
                                                                  else putStrLn $ "Test failed on: " <> show testCase


propIdSymmetrical :: IO ()
propIdSymmetrical = propertyTestUpdate (\s -> s == rot13 (rot13 s)) asciiString 100

propROT13Symmetrical :: IO ()
propROT13Symmetrical = propertyTestUpdate (\s -> s == id (id s)) asciiString 100

--------------------------------------------------------
-- вспомогательный код:
rot13 :: String -> String
rot13 message = caesar 13 message

caesar :: Int -> String -> String
caesar n message = map (\ch -> rotChar n ch) message

rotChar :: Int -> Char -> Char
rotChar n ch | isLower ch = lowerRot n ch
             | isUpper ch = upperRot n ch
             | otherwise = ch

alphabetRot :: [Char] -> Int -> Char -> Char
alphabetRot alphabet n ch =
  alphabet !! ((indexOf ch alphabet + n) `mod` length alphabet)

indexOf :: Char -> [Char] -> Int
indexOf ch [] = undefined
indexOf ch (x : xs) = if x == ch then 0 else 1 + indexOf ch xs

upperRot :: Int -> Char -> Char
upperRot n ch = alphabetRot upperAlphabet n ch

lowerRot :: Int -> Char -> Char
lowerRot n ch = alphabetRot lowerAlphabet n ch

isLower :: Char -> Bool
isLower char = char `elem` lowerAlphabet

isUpper :: Char -> Bool
isUpper char = char `elem` upperAlphabet

lowerAlphabet :: [Char]
lowerAlphabet = ['a' .. 'z']

upperAlphabet :: [Char]
upperAlphabet = ['A' .. 'Z']
