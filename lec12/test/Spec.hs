import MyQC

import Test.QuickCheck
--import Test.QuickCheck.Instances
import Data.Char
import qualified Data.Text as T
import Data.Maybe


-- Программируй на Haskell, Уилл Курт, главы 36, 37
-- (*1)
-- IO-action для модульного тестирования:
assert :: Bool -> String -> String -> IO ()
assert test passStatement failStatement = if test
                                          then putStrLn passStatement
                                          else putStrLn failStatement

main :: IO ()
main = do
{--}
    putStrLn "Running tests...(isPalindrome2)"
    assert (isPalindrome2 "racecar") "passed 'racecar'" "FAIL: 'racecar'"
    assert (isPalindrome2 "racecar!") "passed 'racecar!'" "FAIL: 'racecar!'"
    assert (isPalindrome2 "racecar.") "passed 'racecar.'" "FAIL: 'racecar.'"
    assert ((not . isPalindrome2) "cat") "passed 'cat'" "FAIL: 'cat'"
    putStrLn "done!"
-- stack test

{--}
-- (*2)
    putStrLn "Running tests...(isPalindrome3)"
    assert (isPalindrome3 "racecar") "passed 'racecar'" "FAIL: 'racecar'"
    assert (isPalindrome3 "racecar!") "passed 'racecar!'" "FAIL: 'racecar!'"
    assert (isPalindrome3 "racecar.") "passed 'racecar.'" "FAIL: 'racecar.'"
    assert ((not . isPalindrome3) "cat") "passed 'cat'" "FAIL: 'cat'"
    putStrLn "done!"

{-
QuickCheck - библиотека для автоматического тестирования свойств программ
    1 описать свойства, которым должна удовлетворять функция
    2 QuickCheck генерирует случайные тестовые данные и проверяет выполнение свойств

Тестирование свойств. Формализация функциональности как математического свойства, 
которое должно быть истинным для входов и соответствующих им выходов.
Случайная генерация значений увеличивает шансы обнаружить ошибки, 
так как не зависит от данных, которые определяет разработчик.

Свойство - это характеристика данных, которую можно вычислить и проверить, т.е.
функция которая принимает данные и возвращает булево значение.
-}

{--}
-- (*3)
    -- preprocess1
    putStrLn "Running QuickCheck...(isPalindrome4)"
    quickCheck prop_punctuationInvariant1 -- *** Failed! Falsified ...
    putStrLn "done!"
    putStrLn "Running QuickCheck...(isPalindrome5)"
    quickCheck prop_punctuationInvariant2 --
    putStrLn "done!"
    quickCheckWith stdArgs {maxSuccess = 1000} prop_punctuationInvariant2
    putStrLn "done!"
-- Data.Text -- (*4)
    quickCheckWith stdArgs {maxSuccess = 1000} prop_punctuationInvariant3
    putStrLn "done!"


{--}
-- (*5)
    putStrLn "Running QuickCheck...(isPrime)"
    quickCheck prop_validPrimesOnly
    quickCheckWith stdArgs {maxSuccess = 1000} prop_primesArePrime
    quickCheckWith stdArgs {maxSuccess = 1000} prop_nonPrimesAreComposite
    quickCheck prop_factorsMakeOriginal
    quickCheck prop_allFactorsPrime
    putStrLn "done"

prop_punctuationInvariant1 :: [Char] -> Bool -- тестирование свойств
prop_punctuationInvariant1 text = isPalindrome4 text == isPalindrome4 noPuncText
    where noPuncText = filter (not . isPunctuation) text

prop_punctuationInvariant2 :: [Char] -> Bool
prop_punctuationInvariant2 text = isPalindrome5 text == isPalindrome5 noPuncText
    where noPuncText = filter (not . isPunctuation) text

-- (*4)
prop_punctuationInvariant3 :: T.Text -> Bool
prop_punctuationInvariant3 text = preprocess3 text == preprocess3 noPuncText
    where noPuncText = T.filter (not . isPunctuation) text



---------------------------------------------------------------------
--------------------- Тестирование простых чисел --------------------
---------------------------------------------------------------------

-- isPrime
-- корректность Maybe (получение Nothing и Just) 
prop_validPrimesOnly :: Int -> Bool
prop_validPrimesOnly val = if val < 2 || val > (head . reverse $ primes)
                           then isNothing result
                           else isJust result
    where result = isPrime val

-- проверка простоты на списке от 2 до n-1
prop_primesArePrime :: Int -> Bool
prop_primesArePrime val = if result == Just True then length divisors == 0 else True
    where result = isPrime val
          divisors = filter ((== 0) . (val `mod`)) [2 .. (val - 1)]

-- проверка составных чисел
prop_nonPrimesAreComposite :: Int -> Bool
prop_nonPrimesAreComposite val = if result == Just False then length divisors > 0 else True
    where result = isPrime val
          divisors = filter ((== 0) . (val `mod`)) [2 .. (val - 1)]

-- primeFactors
-- произведение чисел из разложения
prop_factorsMakeOriginal :: Int -> Bool
prop_factorsMakeOriginal val = if result == Nothing
                               then True
                               else product (fromJust result) == val
    where result = primeFactors val

-- проверка элементов разложения на простоту
prop_allFactorsPrime :: Int -> Bool
prop_allFactorsPrime val = if result == Nothing
                           then True
                           else all (== Just True) resultsPrime
    where result = primeFactors val
          resultsPrime = map isPrime (fromJust result)




