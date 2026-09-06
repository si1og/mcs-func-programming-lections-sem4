module MonadLogic where

import Control.Monad.Logic
import Control.Monad ( guard, MonadPlus(..), msum, mplus )

--------------------------------------------------------
{-
Монада Logic

Эффект - недетерминированные вычисления и возможностью вычислений с возвратом (backtracking)
Задачи с поиском множества решений (графы, головоломки, генерация комбинаций, ...)
Logic ленивый и может работать с бесконечным пространством решений и не решений

https://hackage-content.haskell.org/package/logict-0.8.2.0/docs/Control-Monad-Logic.html
https://hackage-content.haskell.org/package/logict-0.8.2.0/docs/src/Control.Monad.Logic.html
https://hackage.haskell.org/package/logict-0.6/docs/Control-Monad-Logic.html#t:LogicT
https://hackage.haskell.org/package/logict
https://okmij.org/ftp/papers/LogicT.pdf
https://hoogle.haskell.org/?hoogle=Logic%20

Тип Logic
newtype Logic a = Logic ( runLogic :: forall r. (a -> r -> r) -> r -> r )


Интерфейсы Logic:
logic :: (forall r. (a -> r -> r) -> r -> r) -> Logic a
            🠕
            обеспечивает полиморфизм для любого типа аккумулятора r
runLogic :: Logic a -> (a -> r -> r) -> r -> r


                  функция, котораяя последовательно применяется ко всем решениям
                  🠗 аккумулятор
                  🠗 🠗
runLogic (logic (\k z -> foldr k z ["asd", "fg", "hj"])) (++) "kl"
                 -------------------------------------
                                   🠕
        сборка логики для прохождения по данным для поиска решения
              (итерирование по множеству возможных ответов)     



instance Monad (LogicT m) where
    return = pure
          -- pure a = LogicT $ \sk fk -> sk a fk
    m >>= f = LogicT $ \sk fk -> unLogicT m (\a fk' -> unLogicT (f a) sk fk') fk



Стандартный интерфейс Logic:
    observe     - Извлекает первый результат из вычисления                      :: Logic a -> a
        observe (return 1 `mplus` return 2)
    observeAll  - Извлекает все результаты из вычисления в виде списка          :: Logic a -> [a]
        observeAll (return 1 `mplus` return 2)
    observeMany - Извлекает первые n результатов из вычисления                  :: Int -> Logic a -> [a]
        observeMany 2 (return 1 `mplus` return 2 `mplus` return 3)

    msplit      - Разделяет вычисление на первый результат и оставшуюся часть   :: Logic a -> Maybe (a, Logic a)
        msplit (return 1 `mplus` return 2)
    ifte        - Выполняет ветвление в вычислениях                             :: Logic a -> (a -> Logic b) -> Logic b -> Logic b
        observeAll (ifte (return 1) (\x -> return (x + 1)) (return 0))

    interleave  - Лениво объединяет (комбинирует) два вычисления                :: Logic a -> Logic a -> Logic a
        observeAll (interleave (return 1) (return 2))
    >>-         - Лениво связывает (комбинирует) вычисления                     :: Logic a -> (a -> Logic b) -> Logic b
        observeAll ((return 1 `mplus` return 2) >>- \x -> return (x + 1))


Порядок прохождения списков для `mplus` и `interleave`: 
[1,2] `mplus` [3,4]
[1,2] `interleave` [3,4]





Задача поиска путей в ориентированном графе:

                    граф как список ребер       принцип обхода графа
                    🠗                           🠗
pathsLogic :: [(Int, Int)] -> Int -> Int -> Logic [Int]
                               🠕      🠕
                               🠕      целевая вершина
                               исходная вершина
-}

pathsLogic :: [(Int, Int)] -> Int -> Int -> Logic [Int]
pathsLogic edges start end =
    {-
    Версия с do-нотацией:
    let e_paths = do (e_start, e_end) <- choices edges
                     guard $ e_start == start
                     subpath <- pathsLogic edges e_end end
                     return $ start:subpath
    Версия с bind:
    -}
    let e_paths = choices edges >>= \(e_start, e_end) ->
               -- Logic (a,b)   >>= ((a,b) -> ...
                  guard (e_start == start) >>
            -- .. (Bool -> Logic ())) -- результат guard игнорируется, но эффект учитывается
                                      -- (здесь реализуется backtracking, 
                                      -- т.к. неудачный guard отбрасывает всю ветку и возвращает mzero)
                  pathsLogic edges e_end end >>= \subpath -> 
               -- Logic [Int]                >>= ([Int] -> ...
                    return $ start:subpath
               -- ... Logic [Int])
    in if start == end then return [end] `mplus` e_paths else e_paths

choices :: [a] -> Logic a
choices = msum . map return
{-
Функция choices превращает список в множество исходов:
:t (map return) - возвращает каждый элемент списка
:t msum - объединяет все случаи

Композиция функций реализует проход по всему списку:
:t (runLogic (choices [(1,2),(2,3),(2,4),(4,2),(1,3)]))
-}


pathsLogicFixed :: [(Int,Int)] -> Int -> Int -> Logic [Int]
pathsLogicFixed edges start end =
    let e_paths = choices edges >>- \(e_start, e_end) -> -- (>>-) :: m a -> (a -> m b) -> m b
                  guard (e_start == start) >>
                  pathsLogicFixed edges e_end end >>- \subpath ->
                    return $ start:subpath
    in if start == end then return [end] `interleave` e_paths else e_paths


mainLogic :: IO ()
mainLogic = do
    let graph2 = [(1,2),(2,3),(2,4),(4,2),(1,3)]
    print (observeMany 5 $ pathsLogic graph2 1 3)
    print (observeMany 5 $ pathsLogicFixed graph2 1 3)
