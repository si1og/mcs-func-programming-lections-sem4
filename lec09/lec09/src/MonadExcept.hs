module MonadExcept where

import Control.Monad.Except ( Except, runExcept,  MonadError(catchError, throwError) )
import Control.Monad ( guard, MonadPlus(..), msum, mfilter )
import Control.Applicative ( Alternative(empty, (<|>)), asum )

--------------------------------------------------------
{-
Обработка исключительных ситуаций


Alternative - расширение Applicative моноидальной операцией

class Applicative f => Alternative f where
    empty :: f a                -- нейтральный элемент
    (<|>) :: f a -> f a -> f a  -- бинарная моноидальная операция

instance Alternative [] where
    empty = []
    (<|>) = (++)

instance Alternative Maybe where
    empty = Nothing
    Nothing <|> m = m
    m@(Just _) <|> _ = m

Nothing <|> Just 3 <|> Just 5 <|> Nothing



Alternative для монады IO
(<|>) при ошибке в левом аргументе обрабатывает ее и запускает правый
empty -- возбуждает исключение ввода-вывода

instance Alternative IO where
    empty :: IO a
    empty = failIO "mzero"

    (<|>) :: IO a -> IO a -> IO a
    m <|> n = m `catchException` \ (_ :: IOError) -> n

readFile "README.md" <|> return "File doesn't exist."
readFile "notREADME.md" <|> return "File doesn't exist."





--------------------------------------------------------
MonadPlus - расширение Alternative до монад

class (Alternative m, Monad m) => MonadPlus m where
    mzero :: m a
    mzero = empty -- пустой или неудачный элемент

    mplus :: m a -> m a -> m a
    mplus = (<|>) -- комбинация двух монадических результатов

Минимальное полное определение:
instance MonadPlus [] -- все берется из базовых классов
instance MonadPlus Maybe
instance MonadPlus IO


Законы MonadPlus (в дополнение к моноидальным законам):
1. mzero - нулевой (убивающий) элемент (аналог нейтрального элемента для сложения)
    mzero >>= k ≡ mzero
    v >> mzero ≡ mzero

2 или. Левая дистибутивность (Left Distribution)
    (a `mplus` b) >>= k ≡ (a >>= k) `mplus` (b >>= k)

или 2'. Левый кетч (Left Catch) (аналог нейтрального элемента для умножения)
    return a `mplus` b ≡ return a

Какие типы удовлетворяют какой версии законов:
    Left Distribution:  []
    Left Catch:         Maybe, IO and STM (Software Transactional Memory)

-- Примеры использования законов:
-}
m1 = [1, 2]
n1 = [3, 4]
k1 x = [x * 2]
leftD1 = (m1 `mplus` n1) >>= k1 -- работает версия 2
leftD2 = (m1 >>= k1) `mplus` (n1 >>= k1) -- работает версия 2

m2 = Just 1
n2 = Just 3
k2 x = Just (x * 2)
leftC1 = (m2 `mplus` n2) >>= k2 -- работает версия 2'
leftC2 = (m2 >>= k2) `mplus` (n2 >>= k2) -- работает версия 2'





--------------------------------------------------------
{-
:i guard
guard :: Alternative f => Bool -> f ()
guard True = pure () -- аналог единицы (нейтральный элемент для аппликативного функтора)
guard False = empty -- аналог нуля (нейстральный элемент для дополнительной моноидальной операции)
-}
pythags = do z <- [1..]
             x <- [1..z]
             y <- [x..z]
             guard (x^2 + y^2 == z^2) -- фильтр с использованием guard
             return (x, y, z)
-- take 5 pythags

{-
                                          Alternative контейнер
                                          🠗       🠗
asum :: (Foldable t, Alternative f) => t (f a) -> f a
                                       🠕
                                       Foldable контейнер
asum = foldr (<|>) empty

asum [Nothing, Just 2, Just 3]
asum [[ ], [2,3], [4]]



msum :: (Foldable t, MonadPlus m) => t (m a) -> m a -- эквивалент asum 
msum = asum -- foldr mplus mzero

msum [Nothing, Just 5, Just 6]
msum [[], [7,8], [9]]



применение предиката к значениям в монадическом контексте m
                                         m a -- значение в контексте
mfilter :: MonadPlus m => (a -> Bool) -> m a -> m a
                          (a -> Bool) -- зависимость эффекта от значения
mfilter p ma = do
    a <- ma
    if p a
        then return a
        else mzero

mfilter (> 3) (Just 5)
mfilter (> 3) (Just 2)
mfilter even [1,2,3,4,5]

-}



--------------------------------------------------------
{-
Монада Except (на основе типа Either)
Эффект - завершение с ошибкой. Вычисление допускает возбуждение и перехват исключений

newtype Except e a = Except { runExcept :: Either e a }
except :: Either e a -> Except e a
except = Except

instance Monad (Except e) where
    return :: a -> Except e a
    return a = except $ Right a

    (>>=) :: Except e a -> (a -> Except e b) -> Except e b
    m >>= k = case runExcept m of
    Left e -> except $ Left e   -- ошибка останавливает вычисление (тривиальное вычисление)
    Right x -> k x  -- вынимаем значение, передаем в стрелку Клейсли

-- Стандартный интерфейс:

throwE :: e -> Except e a -- возбуждение исключительной ситуации
throwE = except . Left -- упаковка объекта ошибки в упаковку except


            возбуждение исключительной ситуации
            🠗
runExcept $ throwError "Error"
🠕                       🠕
🠕                       содержание ошибки
запуск вычисления в монаде


catchE :: Except e a -> (e -> Except e' a) -> Except e' a -- перехват ошибки
--       (Except e a)                                     -- принимает вычисление в монаде и 
--                   -> (e -> Except e' a)                -- обработчик исключительной ситуации 
--                                         -> Except e' a -- e' - другой тип ошибки 
m `catchE` h = case runExcept m of
    Left l -> h l -- обработчик ошибки принимает объект ошибки
                  -- может произвести значение типа (a), которое нужно в случае этой исключительной ситуации
                  -- и это значение может быть упаковано в конструктор Right, что восстанавливает общее вычисление
                  -- но можем и упаковать ошибку для внешнего обработчика
    Right r -> except $ Right r -- переупаковка значения r нужно так как изначальный тип (Except e a) 
                                -- отличается от выходного типа (Except e' a) типом ошибки e и e'


            запрос на выполнение вычисления и отслеживание ошибки
            🠗                      стрелка Клейсли (e -> Except e a)
            🠗                      🠗
runExcept $ catchError (return 4) (throwError)
🠕                       🠕
🠕                       создание правильного вычисления (Haskell автоматически выведет тип)
запуск вычисления в монаде Except

runExcept $ catchError (throwError "Error" :: Except String Int) (throwError :: String -> Except String Int)


try - это упаковка Except или do
do { action1; action2; action3 } `catchE` handler


Есть две библиотеки для работы с монадами. mtl это надстройка над transformers. Большинство функций повторяются и имеют немного отличные имена
В библиотеке mtl:          throwError и catchError
В библиотеке transformers: throwE     и catchE



Рассмотрим пример в котором отслеживаем деление на ноль:
-}
data DivByError = ErrZero | Oth String deriving (Eq,Show)
                   
(/?) :: Double -> Double -> Except DivByError Double
_ /? 0 = throwError ErrZero -- возбуждение исключительной ситуации 
x /? y = return $ x / y -- возврат результата

example0 :: Double -> Double -> Except DivByError String
example0 x y = action `catchError` handler where
    action = do q <- x /? y -- если деление не удачное то возбуждается исключение и все последующие функции пропускаются
                return $ show q
    handler = return . show -- обработчик
{-
runExcept $ example0 5 2
runExcept $ example0 5 0





Представитель Except для моноида (для использования guard, msum, mfilter из MonadPlus или Alternative).
Нужно сделать объект ошибки (e) представителем моноида:

instance Monoid e => MonadPlus (Except e) where
    mzero = except $ Left mempty -- ошибка по умолчанию
    x `mplus` y = except $ let alt = runExcept y in -- накапливание информации об ошибке слева направо (т.к. это моноид)
        case runExcept x of
            Left e -> either (Left . mappend e) Right alt
            r -> r

mzero — ошибка по умолчанию для guard, задается mempty
mplus — накапливает ошибки слева направо, но если происходит удачная попытка, то возвращает удачу

-}

instance Semigroup DivByError where -- представитель полугрупп для реализации моноида (тут есть проблемы с нейтральным элементом)
    Oth s1 <> Oth s2 = Oth $ s1 ++ s2
    Oth s1 <> ErrZero = Oth $ s1 ++ "zero;"
    ErrZero <> Oth s2 = Oth $ "zero;" ++ s2
    ErrZero <> ErrZero = Oth   "zero;zero"

instance Monoid DivByError where -- предсавитель моноида для guard
    mempty = Oth ""
 
example2 :: Double -> Double -> Except DivByError String
example2 x y = action `catchError` handler where
    action = do
        q <- x /? y
        guard $ y >= 0 -- исключаем деление на отрицательные числа
        return $ show q
    handler = return . show

{-
runExcept $ example2 5 2
runExcept $ example2 5 0
runExcept $ example2 5 (-2)
runExcept $ msum [5/?0, 7/?0, 2/?0] -- накопление ошибок
runExcept $ msum [5/?0, 7/?0, 2/?4]
-}
