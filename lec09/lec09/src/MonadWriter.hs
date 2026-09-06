module MonadWriter where

import Control.Monad.Trans.Writer ( Writer, writer, runWriter, execWriter, writer, censor, listen, listens, tell )
import Data.Maybe ( fromMaybe )
import Data.Monoid ( Sum (..) )

--------------------------------------------------------
{-
Монада Writer. (На основе типа пары)
Эффект - запись в лог


Тип Writer:
newtype Writer w a = Writer { runWriter :: (a, w) }


Интерфейсы Writer:
writer :: (a, w) -> Writer w a -- упаковка
runWriter :: Writer w a -> (a, w) -- распаковка


instance Monoid w => Monad (Writer w) where
    return x = writer (x, mempty) -- упаковка значения и нейтрального элемента

    (>>=) :: (r -> a) -> (a -> (r -> b)) -> (r -> b)
    m >>= k = let (x,u) = runWriter m -- извлекаем пару u - лог, x - значение
                  (y,v) = runWriter $ k x -- передаем значение в стрелку Клейсли вычисляем и извлекаем следующую пару
                in writer (y, u `mappend` v) -- собираю пару и упаковываю


Запуск вычислений:
runWriter :: Writer w a -> (a, w) -- простая распаковка значения и возврат пары
execWriter :: Writer w a -> w -- возвращение записанного лога (а вычисления в силу ленивости будут проигнорированы)


runWriter $ writer ("asd",2)

runWriter (return 3 :: Writer String Int) -- нейтральный элемент ""
runWriter (return 3 :: Writer (Sum Int) Int) -- нейтральный элемент 0
execWriter (return 3 :: Writer (Product Int) Int) -- нейтральный элемент 1


Стандартный интерфейс Writer:
    tell    - записывает лог
    listen  - чтение промежуточного значения
    listens - обработка логов
    censor  - модификация логов
-}

type Vegetable = String
type Price = Double
type Qty = Double
type Cost = Double -- Cost = Qty * Price
type PriceList = [(Vegetable,Price)]

prices :: PriceList
prices = [("Potato",13),("Tomato",55),("Apple",48)]

-- Стандартный интерфейс
-- tell :: Monoid w => w -> Writer w () -- задает вывод (записывает лог) -- интересен только лог, значение выкидывается через ()
addVegetable :: Vegetable -> Qty -> Writer (Sum Cost) (Vegetable, Price) -- пользователь берет товар и количество товара
addVegetable veg qty = do
    let pr = fromMaybe 0 $ lookup veg prices
    let cost = qty * pr
    tell $ Sum cost
    return (veg, pr)
-- runWriter $ addVegetable "Apple" 100
-- runWriter $ addVegetable "Pear" 100

-- симуляция пользователя
myCart0 :: Writer (Sum Cost) [(Vegetable, Price)]
myCart0 = do
    x1 <- addVegetable "Potato" 3.5
    x2 <- addVegetable "Tomato" 1.0
    x3 <- addVegetable "AGRH!!" 1.6
    return [x1,x2,x3]
-- runWriter myCart0
-- execWriter myCart0 -- цены сохраняются внутри монады

-- listen :: Monoid w => Writer w a -> Writer w (a, w) -- чтение промежуточных значений:
myCart1 :: Writer (Sum Cost) [((Vegetable, Price), Sum Cost)]
myCart1 = do
    x1 <- listen $ addVegetable "Potato" 3.5
    x2 <- listen $ addVegetable "Tomato" 1.0
    x3 <- listen $ addVegetable "AGRH!!" 1.6
    return [x1,x2,x3]
-- runWriter myCart1 

-- listens :: Monoid w => (w -> b) -> Writer w a -> Writer w (a, b) -- обработка и фильтрация логов
myCart1' :: Writer (Sum Cost) [((Vegetable, Price), Cost)]
myCart1' = do
    x1 <- listens getSum $ addVegetable "Potato" 3.5
    x2 <- listens getSum $ addVegetable "Tomato" 1.0
    x3 <- listens getSum $ addVegetable "AGRH!!" 1.6
    return [x1,x2,x3]
-- runWriter myCart1'

-- censor :: Monoid w => (w -> w) -> Writer w a -> Writer w a -- модификация лога
myCart0' :: Writer (Sum Cost) [(Vegetable, Price)]
myCart0' = censor (discount 10) myCart0

discount :: Double -> Sum Cost -> Sum Cost
discount proc s = case s of
    Sum x
        | x < 100 -> s
        | otherwise -> Sum $ x * (100 - proc) / 100
-- execWriter myCart0
-- execWriter myCart0'
