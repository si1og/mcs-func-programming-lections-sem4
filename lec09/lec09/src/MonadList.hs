module MonadList where

import MonadId ( Candidate (..), Grade (..), Degree (..), viable )

import qualified Data.Map as Map
import Control.Monad ( guard )

--------------------------------------------------------
{-
Монада []
Эффект - множественные результаты вычислений
(недетерминированное вычисление с нулем или большим числом возможных результатов)

instance Monad [] where
    (>>=) :: [a] -> (a -> [b]) -> [b] -- map k xs :: [[b]]
    xs >>= k = concat (map k xs) -- список списков уплощается через concat
    return :: a -> [a]
    return x = [x]

"abc" >>= replicate 4

instance MonadFail [] where
    fail :: String -> [a]
    fail _ = []

-}

list1 = [(x,y) | x <- [1,2,3] , y <- [1,2], x /= y]

list2 = do 
   x <- [1,2,3] -- три ветки вычислений
   y <- [1,2]   -- две ветки вычислений для каждой ветки из x
   True <- return (x /= y) -- каждая ветка вычислений проверяется
   return (x,y) -- возвращаются пары вычислений прошедшие фильтр

list3 = 
  [1,2,3]       >>= (\x -> 
  [1,2]         >>= (\y -> 
  return (x/=y) >>= (\r -> 
  case r of True -> return (x,y)
            _    -> fail "Ignored")))

{-
Отличие Monad от Applicative:
В монадах результат предыдущего вычисления может влиять на <<структуру>> последующих:
do {a <- [1..3]; b <- [a..3]; return (a,b)}
Для аппликативных функторов такое невозможно, структура результата полностью задана структурой аргументов:
(,) <$> [1..3] <*> [3..3]
(,) <$> [1..3] <*> [2..3]
(,) <$> [1..3] <*> [1..3]
-}





--------------------------------------------------------
-- ----------- Программируй на Haskell (31) ------------
-- ----------------------- Maybe -----------------------
candidate1 :: Candidate
candidate1 = Candidate { candidateId = 1, codeReview = A, cultureFit = A, education = BA }
candidate2 :: Candidate
candidate2 = Candidate { candidateId = 2, codeReview = C, cultureFit = A, education = PhD }
candidate3 :: Candidate
candidate3 = Candidate { candidateId = 3, codeReview = A, cultureFit = B, education = MS }
-- поиск по БД возвратит контекст Maybe
candidateDB :: Map.Map Int Candidate
candidateDB = Map.fromList [ (1,candidate1), (2,candidate2), (3,candidate3) ]

assessCandidateMaybe :: Int -> Maybe String
assessCandidateMaybe cId = do
    candidate <- Map.lookup cId candidateDB -- Monad позволяет абстрагироваться от контекста в котором работаем
    let passed = viable candidate
    let statement = if passed
                    then "passed"
                    else "failed"
    return statement
-- assessCandidateMaybe 1
-- assessCandidateMaybe 3
-- assessCandidateMaybe 4

-- ------------------------ [] ------------------------

candidates :: [Candidate]
candidates = [candidate1, candidate2, candidate3]

assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do
    candidate <- candidates         -- список является монадой
    let passed = viable candidate   -- здесь
    let statement = if passed       -- и здесь обращаемся со списком как с одиночным значением
                    then "passed"
                    else "failed"
    return statement                -- а возвращаем список значений
-- assessCandidateList candidates


-- объединение в монадическую функцию
assessCandidate :: Monad m => m Candidate -> m String -- обобщение до всех контекстов
assessCandidate candidates = do
    candidate <- candidates
    let passed = viable candidate
    let statement = if passed
                    then "passed"
                    else "failed"
    return statement
-- assessCandidate readCandidate -- 1 A B PhD
-- assessCandidate (Map.lookup 1 candidateDB)
-- assessCandidate candidates


-- ----------- Программируй на Haskell (32) ------------
-- монады списков как генераторы списков

powersOfTwo :: Int -> [Int]
powersOfTwo n = do
    value <- [1 .. n] -- здесь список - контекст [] значения Int
    return (2^value)
-- powersOfTwo 10

powersOfTwoMap :: Int -> [Int]
powersOfTwoMap n = map (\x -> 2^x) [1 .. n] -- здесь список - структура данных
-- powersOfTwoMap 10

-- do-нотация в синтаксисе генераторов:
powersOfTwo' :: Int -> [Int]
powersOfTwo' n = [value ^ 2 | value <- [1 .. n]] -- [ результат | действия ведущие к результату ]
-- powersOfTwo' 10

-- списки в do-нотации
powersOfTwoAndThree :: Int -> [(Int,Int)]
powersOfTwoAndThree n = do
    value <- [1 .. n]                   -- один список
    let powersOfTwo = 2^value           -- одно значение
    let powersOfThree = 3^value         -- одно знаение
    return (powersOfTwo,powersOfThree)  -- список пар
-- powersOfTwoAndThree 5

-- генератор
powersOfTwoAndThree' :: Int -> [(Int,Int)]
powersOfTwoAndThree' n = [ (powersOfTwo,powersOfThree) | value <- [1 .. n], let powersOfTwo = 2^value, let powersOfThree = 3^value ]
-- powersOfTwoAndThree' 5

allEvenOdds :: Int -> [(Int,Int)]
allEvenOdds n = do
    evenValue <- [2,4 .. n]
    oddValue <- [1,3 .. n]
    return (evenValue,oddValue)
-- allEvenOdds 5

allEvenOdds' :: Int -> [(Int,Int)]
allEvenOdds' n = [(evenValue,oddValue) | evenValue <- [2,4 .. n], oddValue <- [1,3 .. n]]
-- allEvenOdds' 5

powersOfTwoAndThree'' :: Int -> [(Int,Int)]
powersOfTwoAndThree'' n = do
    value1 <- [1 .. n]
    value2 <- [1 .. n]
    let powersOfTwo = 2^value1
    let powersOfThree = 3^value2
    return (powersOfTwo,powersOfThree)
-- powersOfTwoAndThree'' 5

valAndSquare :: [(Int,Int)]
valAndSquare = do
    val <- [1 .. 10]
    return (val,val^2)
-- valAndSquare

-- фильтрация в контексте
evensGuard :: Int -> [Int]
evensGuard n = do
    value <- [1 .. n]
    guard (even value) -- функция guard, для фильтрации
    return value
-- evensGuard 20
-- :t guard

evensGuard' :: Int -> [Int]
evensGuard' n = [ value | value <- [1 .. n], even value] -- guard спрятан в деталях реализации
