module MyQC where

import Data.Char
import qualified Data.Text as T
import Test.QuickCheck

--------------------------------------------------------
{-
Уровни тестирования проекта:
Уровень 1: Тестирование работоспособности ситемы
    проверка выполнения всех требований к системе согласно спецификации
Уровень 2: Тестирование работоспособности модулей
    проверка корректноси взаимодействия модулей
Уровень 3: Тестирование работоспособности функций/алгоритмов/отдельных юнитов
    проверка составляющих модулей на корректность работы

Проблемы ручного тестирования:
    - каждый тест и каждое входное значение пишется вручную
    - покрытие тестами коррелирует с количеством тестов
    - тестирование граничных условий устанавливается вручную (и не всегда правильно)

Тестирование свойств
    - проверка кода как математической функции
    - использование случайных значений как входных параметров



Простые объяснения работы базовых функций QuickCheck:
https://www.youtube.com/watch?v=pD7GmxBA_Qw



Проверка свойств простых функций:
    проверим функцию добивления головы в начало списка:
-}
prop1 x xs = (length xs) + 1 == (length $ x:xs)
{-
quickCheck prop1

    проверим функцию отделения хвоста от списка:
-}
prop2 xs = (length $ tail xs) == ((length xs) - 1)
{-
quickCheck prop2
*** Failed! (after 1 test): ...

Исключим из тестируемых данных случай, которые не имеют физического смысла:
-}
prop3 xs = not (null xs) ==> (length $ tail xs) == ((length xs) - 1)
{-                        🠕
            оператор из синтаксиса QuickCheck
quickCheck prop3

+++ OK, passed 100 tests; 15 discarded.
                          -------------
                                🠕
                        отклоненные случаи


quickCheck (verbose prop3)
                🠕
        полный вывод кейсов


В действительности нам не нужно проверять истинность применение функций
Хотим проверить эквивалентность данных, которые передаются
    используем оператор (===)
-}
prop4 xs = not (null xs) ==> (length $ tail xs) === ((length xs) - 1)
{-
quickCheck prop4


Протестируем собственную функцию reverse
-}
rev xs = rev_aux [] xs
    where 
        rev_aux acc [] = acc
        rev_aux acc (x:xs) = rev_aux (x:acc) xs

propRev1 xs = collect (length xs) $ reverse xs === rev xs
{-              🠕
собираем дополнительную статистику по длине входных данных

quickCheck propRev1

Пустые списки тривиальные ограничим их использование в тестах:
-}
propRev2 xs = classify (length xs == 0) "empty" $ reverse xs === rev xs
{-
quickCheck propRev2
Собрали статистику по пустым спискам

расширение числа тестов:
quickCheck (withMaxSuccess 10000 propRev2)



QuickCheck не всегда генерирует полезные данные для проверки функций
    нужно использовать конкретные типы для для содержательного тестирования
-}
propLook1 k v m = lookup k ((k,v) : m) === Just v
{-
quickCheck (verbose propLook1)
    тестирует не то, что нужно. просто проверяет ()

напрямую запишем типы:
-}
propLook2 k v m = lookup k ((k,v) : m) === Just v
    where types = (k :: Int, v :: Int) -- значение не используется, просто маркерует типы
{-
quickCheck (verbose propLook2)
-}





-- Программируй на Haskell, Уилл Курт, главы 36, 37
--------------------------------------------------------
----------------- Ручное тестирование ------------------
--------------------------------------------------------

isPalindrome1 :: String -> Bool
isPalindrome1 text = text == reverse text

{-
isPalindrome1 "racecar"
isPalindrome1 "cat"
isPalindrome1 "racecar!" -- !
-}

isPalindrome2 :: String -> Bool
isPalindrome2 text = cleanText == reverse cleanText
    where cleanText = filter (not . (`elem` ['!'])) text
-- isPalindrome2 "racecar!"



--------------------------------------------------------
------------------- Модульные тесты --------------------
--------------------------------------------------------

-- Spec.hs (*1)

isPalindrome3 :: String -> Bool
isPalindrome3 text = cleanText == reverse cleanText
    where cleanText = filter (not . (`elem` ['!','.'])) text

-- Spec.hs (*2)

isPalindrome4 :: String -> Bool
isPalindrome4 text = cleanText == reverse cleanText
    where cleanText = preprocess1 text

preprocess1 :: String -> String
preprocess1 text = filter (not . (`elem` ['!','.'])) text --,'(',')','{','}',';','%','\"','/','-'

-- Spec.hs (*3)

isPalindrome5 :: String -> Bool
isPalindrome5 text = cleanText == reverse cleanText
    where cleanText = preprocess2 text

preprocess2 :: String -> String
preprocess2 text = filter (not . isPunctuation) text

-- (*4)
--------------------------------------------------------
{-
Все типы, для которых QuickCheck может автоматически создавать значения, должны быть представителсями класса Arbitrary
По умолчанию у Data.Text нет представителя в Arbitrary
Но можно использовать пакет quickcheck-instances
    (это пакет не очень хорошо поддерживается и может временами вызывать конфликты версий)

Или написать представителя руками:
-}

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary
  shrink t = T.pack <$> shrink (T.unpack t)

preprocess3 :: T.Text -> T.Text
preprocess3 text = T.filter (not . isPunctuation) text

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText
    where cleanText = preprocess3 text



--------------------------------------------------------
--------------- Тестирование простых чисел -------------
--------------------------------------------------------

primes :: [Int]
primes = sieve [2 .. 10000]

sieve :: [Int] -> [Int]
sieve [] = []
sieve (nextPrime:rest) = nextPrime : sieve noFactors
    where noFactors = filter (not . (== 0) . (`mod` nextPrime)) rest

-- Just True - простое число
-- Just False - составное число
-- Nothing - исключения (отрицательные числа)
isPrime :: Int -> Maybe Bool
isPrime n | n <= 1 = Nothing
          | n > (head . reverse $ primes) = Nothing
          | otherwise = Just (n `elem` primes)

unsafePrimeFactors :: Int -> [Int] -> [Int]
unsafePrimeFactors 0 [] = []
unsafePrimeFactors n [] = []
unsafePrimeFactors n (next : primes) = if n `mod` next == 0
                                       then next : unsafePrimeFactors (n `div` next) (next : primes)
                                       else unsafePrimeFactors n primes

primeFactors :: Int -> Maybe [Int]
primeFactors n | n < 2 = Nothing
               | n >= (head . reverse $ primes) = Nothing
               | otherwise = Just (unsafePrimeFactors n primesLessThanN)
    where primesLessThanN = filter (<= n) primes

-- (*5)

