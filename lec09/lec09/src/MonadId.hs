module MonadId where

import Control.Monad.Identity (Identity(..))

--------------------------------------------------------
{-
Монада  Identity
Эффект - отсутствие эффекта. Хранит значение в контекстеи возвращает без изменений

newtype Identity a = Identity { runIdentity :: a }


Оборачиваение значения в контекст:
Identity (3 :: Int)
Извлечение значения из контекста:
runIdentity $ Identity (3 :: Int)



instance Monad Identity where
    (>>=) :: Identity a -> (a -> Identity b) -> Identity b
    Identity x >>= k = k x
          -- m >>= k = k (runIdentity m)

    return :: a -> Identity a
    return = Identity


Простая функция для реализации стрелки Клейсли (принимает значение и прибавляет к нему единицу):
-}
wrap'n'succ :: Integer -> Identity Integer
wrap'n'succ x = Identity (succ x)
{-
Формирование цепочек монадических вычислений:
runIdentity $ wrap'n'succ 3
runIdentity $ wrap'n'succ 3 >>= wrap'n'succ >>= wrap'n'succ



Законы класса типов Monad:
    1. return a >>= k ≡ k a
    2. m >>= return ≡ m
    т.е. 1 и 2 законы говорят, что return не вносит новых эффектов (return - нейтральный элемент)

runIdentity $ wrap'n'succ 3
runIdentity $ return 3 >>= wrap'n'succ
runIdentity $ wrap'n'succ 3 >>= return

    3. (m >>= k) >>= k'   ≡   m >>= (\x -> k x >>= k')
    Ассоциативности нет, т.е. так написать нельзя: m >>= (k >>= k') - см типы
       (m >>= k) >>= k'
          m b
                              m >>= (k >>= k') -- типы не сходятся с (>>=)
                                     k :: a -> m b
                              m >>= (\x -> (k x >>= k'))

runIdentity $ wrap'n'succ 3 >>= wrap'n'succ >>= wrap'n'succ
runIdentity $ wrap'n'succ 3 >>= (\x -> wrap'n'succ x >>= wrap'n'succ)



Третий закон монад:
-}
goWrap0 :: Identity Integer
goWrap0 = wrap'n'succ 3 >>= 
          wrap'n'succ >>= 
          wrap'n'succ >>= 
          return

-- императивная последовательность инструкций:
goWrap1 :: Identity Integer
goWrap1 = wrap'n'succ 3 >>= (\x ->  -- x = wrap'n'succ 3;
          wrap'n'succ x >>= (\y ->  -- y = wrap'n'succ x;
          wrap'n'succ y >>= (\z ->  -- z = wrap'n'succ y;
          return z)))               -- return z;

goWrap2 :: Identity (Integer,Integer,Integer)
goWrap2 = wrap'n'succ 3 >>= (\x ->  -- x = wrap'n'succ 3;
          wrap'n'succ x >>= (\y ->  -- y = wrap'n'succ x;
          wrap'n'succ y >>= (\z ->  -- z = wrap'n'succ y;
          return (x,y,z))))         -- return (x,y,z);

-- абстрагирование и связывание с чистыми вычислениями:
goWrap3 :: Identity (Integer,Integer,Integer,Integer)
goWrap3 = let i = 3 in              -- i = 3; -- чистое вычисление
          wrap'n'succ i >>= (\x ->  -- x = wrap'n'succ i;
          wrap'n'succ x >>= (\y ->  -- y = wrap'n'succ x;
          wrap'n'succ y >>= (\z ->  -- z = wrap'n'succ y;
          return (i,x,y,z))))       -- return (i,x,y,z);





--------------------------------------------------------
{-
Засахаривание и рассахаривание do-нотации

- Значение в монадическом контексте (e :: m a):
do {e}                  ≡ e 
- Сохранение эффекта и игнорирование значения:
do {e; stmts}           ≡ e >> do {stmts}
- Использование значения в контексте в вычислении с контексте:
do {p <- e; stmts}      ≡ e >>= \p -> do {stmts}
- Использование чистого значения в вычислении в контексте:
do {let v = exp; stmts} ≡ let v = exp in do {stmts}
-}

goWrap4 :: Identity (Integer,Integer,Integer)
goWrap4 = let i = 3 in
          wrap'n'succ i >>= (\x -> 
          wrap'n'succ x >>= (\y -> 
          wrap'n'succ y >> -- облегченный bind если результат не интересен
          return (i,x,y)))

goWrap5 :: Identity (Integer,Integer,Integer)
goWrap5 = do 
          let i = 3
          x <- wrap'n'succ i
          y <- wrap'n'succ x
          wrap'n'succ y
          return (i,x,y)





-- запись с использованием монадического баинда
askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!" 

helloName :: IO ()
helloName = askForName >>                -- *1
    getLine >>= (\name ->                -- *2
        return (nameStatement name)) >>= -- *3
    putStrLn                             -- *3

-- запись с использованием do-notation
helloNameDo :: IO ()
helloNameDo = do
    askForName                           -- *1
    name <- getLine                      -- *2
    putStrLn (nameStatement name)        -- *3


-- случаи с более простым bind чем do-нотацией
echo :: IO ()
echo = getLine >>= putStrLn

echoDO :: IO ()
echoDO = do
    val <- getLine
    putStrLn val





--------------------------------------------------------
{-
Программа проверяет подходит ли кандидат под вакансию по нескольким критериям
-}

data Grade = F | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)

data Degree = HS | BA | MS | PhD deriving (Eq, Ord, Enum, Show, Read)

data Candidate = Candidate { candidateId :: Int
    , codeReview :: Grade
    , cultureFit :: Grade
    , education :: Degree } deriving Show

viable :: Candidate -> Bool
viable candidate = all (== True) tests
    where passedCoding = codeReview candidate > B
          passedCultureFit = cultureFit candidate > C
          educationMin = education candidate >= MS
          tests = [passedCoding, passedCultureFit, educationMin]

testCandidate :: Candidate
testCandidate = Candidate { candidateId = 1
    , codeReview = A
    , cultureFit = A
    , education = PhD }
-- viable testCandidate

readInt :: IO Int
readInt = getLine >>= (return . read)
-- readInt :: IO Int
-- readInt = do
--     input <- getLine
--     return (read input)

readGrade :: IO Grade
readGrade = getLine >>= (return . read)
-- readGradeDo :: IO Grade
-- readGradeDo = do
--     input <- getLine
--     return (read input)

readDegree :: IO Degree
readDegree = getLine >>= (return . read)
-- readDegree :: IO Degree
-- readDegree = do
--     input <- getLine
--     return (read input)

readCandidate :: IO Candidate
readCandidate = do
    putStrLn "enter id:"
    cId <- readInt
    putStrLn "enter code grade:"
    codeGrade <- readGrade
    putStrLn "enter culture fit grade:"
    cultureGrade <- readGrade
    putStrLn "enter education:"
    degree <- readDegree
    return (Candidate { candidateId = cId       -- тип Candidate не является IO!, работоспособность поддерживает класс типов Monad
                      , codeReview = codeGrade
                      , cultureFit = cultureGrade
                      , education = degree })

assessCandidateIO :: IO String
assessCandidateIO = do
    candidate <- readCandidate
    let passed = viable candidate
    let statement = if passed
                    then "passed"
                    else "failed"
    return statement
-- assessCandidateIO -- 1 A B PhD
