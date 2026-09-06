module Pizza ( pizza ) where

{-
Реализуем функцию, которая определяет самую дешевую пиццу из расчета на квадратный сантиметр
-}

type Pizza = (Double, Double)

areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi * ((size / 2) ** 2)

costPerCm :: Pizza -> Double
costPerCm (size, cost) = cost / areaGivenDiameter size

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 = if costP1 < costP2
                      then p1
                      else p2
    where costP1 = costPerCm p1
          costP2 = costPerCm p2

describePizza :: Pizza -> String
describePizza (size, cost) = "Pizza size " ++ show size ++ " is chipper " ++ show costSqCm ++ " for square santimeters"
    where costSqCm = costPerCm (size, cost)

pizza :: IO ()
pizza = do
    putStrLn "Enter size of a first pizza:"
    size1 <- getLine
    putStrLn "Enter cost of a first pizza:"
    cost1 <- getLine
    putStrLn "Enter size of a second pizza:"
    size2 <- getLine
    putStrLn "Enter cost of a second pizza:"
    cost2 <- getLine
    let pizza1 = (read size1, read cost1)
    let pizza2 = (read size2, read cost2)
    let betterPizza = comparePizzas pizza1 pizza2
    putStrLn (describePizza betterPizza) 
