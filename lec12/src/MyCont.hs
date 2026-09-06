{-# LANGUAGE InstanceSigs #-}

module MyCont where

import Control.Monad (when)

--------------------------------------------------------
{-
Монада Cont
    Эффект управления продолжением вычислений

https://hackage-content.haskell.org/package/transformers-0.6.3.0/docs/src/Control.Monad.Trans.Cont.html
https://hackage-content.haskell.org/package/mtl-2.3.2/docs/Control-Monad-Cont.html


Continuation-passing style (CPS)

Возьмем пару функций:
-}
square :: Int -> Int
square x = x ^ 2

add :: Int -> Int -> Int
add x y = x + y
{-
Для представления этих функций в стиле CPS
Нужно добавить аргумент называемый continuation:
-}
squareCont :: Int -> (Int -> r) -> r
squareCont x c = c $ x ^ 2

addCont :: Int -> Int -> (Int -> r) -> r
addCont x y c = c $ x + y
{-
Отличие работы функций в разных стилях:

square 2

:t squareCont 2

squareCont 2 id 

squareCont 2 show

:t squareCont 2 squareCont

squareCont 2 (addCont 3) (addCont 4) id
    получается композиция функций

Функции с передачей продолжения требует применения функции продолжения,
    но может и сама использоваться в качестве продолжения вычисления



На данный момент вычисления линейны, хотелось бы использовать более сложное поведение с ветвлениями (нелинейные вычисления)

Пример более сложной функции:
-}
sumSqCont :: Int -> Int -> (Int -> r) -> r
sumSqCont x y c =
    squareCont x $ \x2 -> 
{-      🠕          🠕
    используем  | второй  
  функции в CPS | аргумент (функция продолжения)
        🠗          🠗    -}
    squareCont y $ \y2 -> 
{-          🠕        🠕
результат этого      🠕
        вычисления   🠕
        связывается  🠕
                  с этой переменной -}
    addCont x2 y2 $ \z -> 
    c z
{-
sumSqCont 2 3 show

лямбды дают возможность создавать более сложное поведение с ветвлениями



В CPS видны аналогии с монадическим интерфейсом:
    очевидно, return должен помещать значение в контекст с продолжением вычисления
    баинд - это конструкция с лямбдами

Попробуем написать монаду Cont:
-}
newtype Cont r a = Cont { runCont :: (a -> r) -> r }

evalCont :: Cont r r -> r
evalCont m = runCont m id

instance Functor (Cont r) where
    fmap :: (a -> b) -> Cont r a -> Cont r b
    fmap f x = Cont $ \c -> runCont x (c . f)

instance Applicative (Cont r) where
    pure :: a -> Cont r a
    pure x = Cont $ \c -> c x
    
    (<*>) :: Cont r (a -> b) -> Cont r a -> Cont r b
    f <*> v = Cont $ \c -> runCont f $ \g -> runCont v (c . g)


instance Monad (Cont r) where
    return :: a -> Cont r a
    return x = Cont $ \c -> c x

    (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
    Cont v >>= k = Cont $ \c -> v (\a -> runCont (k a) c)
{- Семантика построения монадического баинда: композиция продолжений -}
bind :: ((a -> r) -> r) -> (a -> (b -> r) -> r) -> (b -> r) -> r
bind v k = \c -> v (\a -> k a c)



-- примеры с монадическим синтаксисом:
squareContM :: Int -> Cont r Int
squareContM x = return $ x ^ 2

addContM :: Int -> Int -> Cont r Int
addContM x y = return $ x + y
{-
runCont (squareContM 2) show
evalCont (squareContM 2)
evalCont (addContM 2 3)
evalCont (squareContM 2 >>= (addContM 3) >>= (addContM 4))



Пример ветвления:
-}
sumSmth1 :: Cont r Int
sumSmth1 = do
    a <- return 3
    b <- Cont $ \c -> c 4 {- это return, 
                 🠕    🠕
                 доступ к продолжению -}
    return $ a + b
-- runCont sumSmth1 id

sumSmth2 :: Cont String Int
sumSmth2 = do
    a <- return 3
    b <- Cont $ \c -> "STOP" {- изменение поведения (игнорирование продолжения) -}
    return $ a + b
-- runCont sumSmth2 show

sumSmth3 :: Cont [r] Int
sumSmth3 = do
    a <- return 3
    b <- Cont $ \c -> c 4 ++ c 5 {- множественные вычисления 
                       🠕   🠕  🠕 
              первая ветка | вторая ветка 
               вычислений  |  вычислений
                           |
        результаты конкатенируются (итоговый тип [r])

    каждая ветка вычислений
    выполняет действие (в первой ветке 3 + 4, во второй 3 + 5)
               🠗       -}
    return $ a + b
-- runCont sumSmth3 show



{-
Стандартный интерфейс Cont:
    callCC - call with current continuation
прерывает текущее вычисление и возвращает текущее значение
в отличие от throwError и catchError делает продолжение явным
можно выйти из любого уровня вложенности вычисления
предоставляет большую гибкость и контроль на уровне типов
-}
callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC f = Cont $ \ c -> runCont (f $ \ x -> Cont $ \ _ -> c x) c

callCCbase :: ((a -> (b -> r) -> r) -> (a -> r) -> r) -> (a -> r) -> r
callCCbase f = \c -> f (\a _ -> c a) c


test :: Int -> Cont Int Int
test x = do
    a <- return 3
    Cont $ \c -> if x > 100 then 42 else c () {- условие прерывания вычисления
     🠕              🠕             🠕       🠕
     🠕  проверка параметра x      🠕     вызов продолжения
     🠕                  вычисление прерывается 
     🠕              (!но это значение хардкодит тип 
     🠕                  первого параметра монады) 
    залезли внутрь вычисления, поэтому тип такой -}
    return $ a + x
{-
evalCont $ test 30
evalCont $ test 300
runCont (test 30) show

функция callCC восстанавливает полиморфизм первого параметра монады Cont
-}

testCallCC :: Int -> Cont r Int
testCallCC x = callCC $ \k -> do {-
                            тип принимаемой функции,
        которая возвращает монаду продолжения (по типу совпадает с выходом всей монады)
                                        🠗                           🠗 (! полиморфна по r)
                          -------------------------------           🠗
               callCC :: ((Int -> Cont r b) -> Cont r Int) -> Cont r Int
                          -----------------
                                  🠕
                функция конструируется по месту лямбдами

                здесь k :: Int -> Cont r b
                а do-нотация собирает монаду Cont -}
    a <- return 3
    when (x > 100) (k 42) {- when :: Applicative f => Bool -> f () -> f () если true то возвращает содержательную монаду, в противном случае юнит тайп обернутый в монаду 
                    🠕
    здесь вычисление прерывается и возвращается значение
        если не используем k, то весь блок игнорируется -}
    return $ a + x
{-
runCont (testCallCC 30) id
runCont (testCallCC 30) show
-}
