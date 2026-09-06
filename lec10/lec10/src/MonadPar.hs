{-# LANGUAGE BangPatterns #-}

module MonadPar where

import Control.DeepSeq ( NFData(..), rnf )
import Control.Parallel.Strategies ( Strategy, rdeepseq, using, parList, runEval, rpar, rseq, parTuple2 )
import Control.Parallel ( par, pseq )
import Control.Exception ( evaluate )
import Data.Time.Clock ( getCurrentTime, diffUTCTime, UTCTime )

import Control.Concurrent ( forkIO, ThreadId(..), putMVar, newEmptyMVar, takeMVar, MVar(..), myThreadId, 
                            Chan(..), newChan, writeChan, readChan )
import System.IO

import Control.Concurrent.STM

{-
Библиотека monad-par на данный момент устарела, при использовании могут быть проблемы с зависимостями



Параллелизм (Parallelism)   | Конкурентность (Concurrency)
-----------------------------------------------------------
Одновременное выполнение    | Управление несколькими 
задачи на нескольких        | задачами с переключением 
ядрах для ускорения         | между ними
-----------------------------------------------------------
Результат детерминирован    | Результат не детерминирован
Порядок выполнения не важен | Порядок выполнения важен
Control.Parallel (parallel)	| Control.Concurrent (stm)



Параллелизм
https://www.youtube.com/watch?v=R47959rD2yw

https://hackage-content.haskell.org/package/monad-parallel-0.8.0.1/docs/Control-Monad-Parallel.html
https://hackage-content.haskell.org/package/parallel-3.3.0.0/docs/Control-Parallel-Strategies.html
https://hackage.haskell.org/package/parallel-2.0.0.0/docs/Control-Parallel-Strategies.html
https://hackage-content.haskell.org/package/deepseq-1.5.2.0/docs/Control-DeepSeq.html


Конкурентность
https://www.youtube.com/watch?v=cuHD2qTXxL4

https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Control-Concurrent.html


STM (Software Transactional Memory)
https://www.youtube.com/watch?v=2lll2VbX8Vc

https://hackage.haskell.org/package/stm-2.5.3.1/docs/Control-Monad-STM.html



Монада Eval - вычисление, которое может быть параллельным или последовательным. Управляется RTS (real-time system)
              монада параллельного вычисления (строгая монада Id)
              для (m >>= f) гарантирует вычисления m ДО передачи результата в f

type Strategy a = a -> Eval a - тип который можно композировать
                                т.е. можно сконструировать сложные последовательности вычислений 
                                часть которых могут быть параллельными, а другая последовательными
                                ! описывает как вычислить данные, но не привязана к данным как таковым !


:t rpar :: Strategy a
           a -> Eval a
    ! rpar требует конкретный тип на входе, не thunk !
    создает spark (спарк, задачу) для runtime system из выражения a
    поток НЕ блокируется и возвращается Eval a
    RTS самостоятельно выбирает ядро и время вычисления задачи a

:t rseq :: Strategy a
    вычисляет аргументы до слабой головной нормальной формы

runEval :: Eval a -> a - запуск вычисления

dot :: Strategy a -> Strategy a -> Strategy a 
    композитор стратегий для сборки сложных процессов вычислений


-}

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

mainPar :: IO ()
mainPar = do
    let (a,b) = runEval $ do
            a <- rpar $ fib 38 -- спарк 1 (Spark)
            b <- rpar $ fib 37 -- спарк 2 (Spark)
                               -- оба спарка отправляются на вычисление в пул спарков
            rseq a -- форсирование вычисления спарка 1 (в данном случае не обязательно)
            rseq b -- форсирование вычисления спарка 2 (в данном случае не обязательно)
            return (a,b)
    putStrLn $ show (a,b)
{-
stack exec --rts-options="-N4 -s" lec10-exe
           ----------------------
                     🠕
                     настройки для exe файла

stack exec -- lec10-exe +RTS -N -s
            🠕
            разделитель флагов для stack (без него флаги +RTS -N -s отправятся в stack и игнорируются, с ним применятся к exe файлу)


...
          создано спарков как в коде mainPar
          🠗                             пустые спарки
          🠗                             🠗      уничтоженные спарки
          🠗                             🠗      🠗
  SPARKS: 2 (1 converted, 0 overflowed, 0 dud, 0 GC'd, 1 fizzled)
             🠕            🠕                             🠕
             🠕            🠕                             отмененные спарки (из-за порядка и времени выполнения выполнения)
             🠕            вытеснено из пула спарков
             один спарк выполнен на ядре

...
  MUT     time    0.766s  (  0.468s elapsed) - реальное время выполнения программы
...




Data Parallelism - независимое выполнение одного участка кода над разными участками данных параллельно
-}

findFactors :: Integer -> [Integer]
findFactors n = filter (\d -> n `mod` d == 0) [1..n] `using` parList rdeepseq

{-
Стратегия для распараллеливания вычислений в списках
:t parList      :: Strategy a -> Strategy [a]
    разбивает список для параллельных вычислений

     calculation :: Strategy a
          |
  --------|--------
  🠗     🠗   ...   🠗
calc1 calc2 ... calcN  :: Strategy [a]


:t rdeepseq     :: NFData a => Strategy a
    рекурсивные вычисления до нормальной формы
    стратегия вычисления значения до нормальной формы

parList rdeepseq - стратегия параллельной обработки списков

:t using        :: a -> Strategy a -> a
    примененеи стратегии
    сахар для x `using` s = runEval (s x)

NFData - класс типов гарантирующий вычисление до нормальной формы



`using` parList rdeepseq - гарантирует использование строгих стратегий и распараллеливание вычисления списка [1..n]

findFactors 10000001
-}


findThreeFactors :: Integer -> Integer -> Integer -> ([Integer], [Integer], [Integer])
findThreeFactors x y z = runEval $ do
    fx <- rpar (rnf (findFactors x) `seq` findFactors x)
    fy <- rpar (rnf (findFactors y) `seq` findFactors y)
    fz <- rpar (rnf (findFactors z) `seq` findFactors z)
    return (fx, fy, fz)

{-
:t rnf  :: NFData a => a -> ()
    разворачивает структуру до нормальной формы, 
    вычисляет все элементы
    () - метка окончания работы


               это один и тот же thunk 
                🠗                    🠗
           -------------        -------------
rpar (rnf (findFactors x) `seq` findFactors x)
  🠕    🠕                    🠕
  🠕    убирает thunk        включает rnf для получения нормальной формы
  отправляет вычисление без ожидания результата в runtime system

В do нотации собирается три параллельных вычисления fx, fy, fz по средствам функции rpar
return и runEval блокируют вычисления до тех пор пока не будут получены все три результата

распределение по ядрам обеспечивает runtime system





Общий паттерн параллельных вычислений:
    запускает два вычисления в параллель, возвращает только второе (первое вычисление может быть расходящимся):
    🠗
x `par` y `pseq` someFunc x y
            🠕
            похож на seq (seq строг по обоим аргументам)
            pseq строг только по первому аргументу (для контроля последовательности выполнения действия в ходе распараллеливания вычислений)

undefined `par` 1
1 `par` undefined

1 `pseq` undefined
undefined `pseq` 1
-}

findTwoFactors x y = 
    let !fx = findFactors x -- {-# LANGUAGE BangPatterns #-}
        !fy = findFactors y -- BangPatterns (!) — принудительное вычисление (strict evaluation)
    in fx `par` (fy `pseq` (fx, fy))

{-
Стратегии параллельного вычисления для кортежей
parTuple2 :: Strategy a -> Strategy b -> Strategy (a, b)
...
parTuple9 :: Strategy a -> Strategy b -> Strategy c -> Strategy d -> Strategy e -> Strategy f -> Strategy g -> Strategy h -> Strategy i -> Strategy (a, b, c, d, e, f, g, h, i)
-}

findTwoFactors' x y = (findFactors x, findFactors y) `using` parTuple2 rdeepseq rdeepseq

{-
Стратегии для других типов
https://hackage-content.haskell.org/package/parallel-3.3.0.0/docs/Control-Parallel-Strategies.html



Сравнение времени выполнения:
-}

findFactorsSlow :: Integer -> [Integer]
findFactorsSlow n = filter (\d -> n `mod` d == 0) [1..n]

findThreeFactorsSlow :: Integer -> Integer -> Integer -> ([Integer], [Integer], [Integer])
findThreeFactorsSlow x y z = (findFactorsSlow x, findFactorsSlow y, findFactorsSlow z)



timeMeasure name f arg1 arg2 arg3 = do
    startWall <- getCurrentTime
    !result <- evaluate (rnf (f arg1 arg2 arg3))
    stopWall <- getCurrentTime
    let wallTime = diffUTCTime stopWall startWall
    putStrLn $ name ++ " = " ++ show wallTime
    return result

mainParTimeMesure :: IO ()
mainParTimeMesure = do
    let x, y, z :: Integer
        x = 10000000
        y = 10000001
        z = 9999999
    timeMeasure "Slow"     findThreeFactorsSlow x y z
    timeMeasure "Parallel" findThreeFactors     x y z




--------------------------------------------------------
{-
Concurrency (конкуренция)
    использование одного ресурса для решения двух задач
    задачи решаются "параллельно" и могут взаимодействовать в процессе вычисления

Thread - нить/поток, который управляется средой и имеют низкие накладные расходы при вычислении

data ThreadId - абстрактный тип для дескриптора потока


forkIO :: IO () -> IO ThreadId
    создание нового потока (thread) для вычислений типа ввода-вывода
    берет действие и выполняет его в thread

Как создавать нити (threads):
-}

funForThread :: Int -> Int -> IO ()
funForThread x y = do
    let result = x + y
    putStrLn $! show result

mainThread :: IO ()
mainThread = do
    forkIO $ funForThread 1 2 -- :: IO Thread
    return ()

{-
Как происходит взаимодействие между thread?
    У нитей есть разделяемая память MVar (разделяемое мутабельное состояние)

Действия в MVar атомарные (атомарные действия - операция, которая выполняется полностью или не выполняет вообще)
    Например: Есть три нити (thread) которые разделяют доступ к одному MVar
              Доступ к MVar двунаправленный (т.е. каждый может либо поместить что-то в память либо взять)
              Пусть thread_1 и thread_2 хотят поместить что-то в MVar, а thread_3 хочет что-то извлечь
              Происходит выполнение кода:
                1 - thread_1 помещает данные в MVar (thread_1 выполнила свою задачу, действие завершено)
                2? - ! если thread_2 попытается полнить свое действие, то не получится, так как MVar заполнен данными (thread_2 заблокирован, ищем другую нить для выполнения действий ...)
                2 - ... в нашем случае thread_3 может выполнить действие по извлечению данных из MVar (теперь в MVar ничего нет, thread_3 выполнила свою задачу и можно приступить к выполнению thread_2)
                3 - thread_2 помещает свои данные в MVar (задача thread_2 выполнена)
                ... продолжение работы программы


Интерфейс работы с MVar:
newEmptyMVar :: IO (MVar a)     -- создание нового пустого разделяемого мутабельного состояния
newMVar :: a -> IO (MVar a)     -- создание нового разделяемого мутабельного состояния
takeMVar :: MVar a -> IO a      -- атомарная блокирующая операция извлечения данных
putMVar :: MVar a -> a -> IO () -- атомарная блокирующая операция помещения данных


Использование MVar:
-}

funForThreadMVar :: Int -> Int -> MVar Int -> IO ()
funForThreadMVar x y mVar = do
    putMVar mVar $! (x + y)
              -- 🠕
              -- MVar ленивый поэтому нужно форсировать вычисления !
              -- если не использовать $!, то здесь будет создан thunk !
mainThreadMVar :: IO ()
mainThreadMVar = do
    mVar <- newEmptyMVar
    forkIO $ funForThreadMVar 1 2 mVar
    result <- takeMVar mVar
    putStrLn $ show result



{-
Каналы (channels) - FIFO очередь реализованная на MVar и не ограничены (могут разрастаться бесконечно)
тип Chan - абстрактный тип канала

Пример:
    Есть три нити (thread) которые разделяют доступ к одному каналу Chan
    Доступ к MVar двунаправленный (т.е. каждый может либо поместить что-то в память либо взять)
    Пусть thread_1 и thread_2 хотят поместить что-то в MVar, а thread_3 хочет что-то извлечь
    Происходит выполнение кода:
        1 - thread_1 отправляет данные в канал (действие завершено)
        2 - thread_2 отправляет данные в канал (действие завершено) - его данные вторые в очереди
        3 - thread_3 извлекает данные из канала (действие завершено) - забрал данные которые положил thread_1
                                                                       теперь первыми в очереди данные thread_2
        ...

Интерфейс работы с Chan:
newChan :: IO (Chan a)
writeChan :: Chan a -> a -> IO ()
readChan :: Chan a -> IO a

-}

getGreeting :: IO String
getGreeting = do
    tid <- myThreadId
    let greeting = "Hello from " ++ show tid
    return $! greeting

threadHello :: Chan () -> IO ()
threadHello endFlags = do
    greeting <- getGreeting
    putStrLn greeting
    writeChan endFlags () -- флаг выполнения нити (thread)
                          -- в канал записывается юнит тайп ()

mainChan :: IO ()
mainChan = do
    hSetBuffering stdout NoBuffering -- отключение буфферизации
               -- stdout - разделяемый ресурс для всех thread (за него начинается борьба)
    endFlags <- newChan -- создание канала
    forkIO $ threadHello endFlags -- создаем нить и помещаем ее в канал
    forkIO $ threadHello endFlags
    forkIO $ threadHello endFlags
    forkIO $ threadHello endFlags
    forkIO $ threadHello endFlags
    mapM_ (\_ -> readChan endFlags) [1..5] -- читаем каждый юнит тайп () из канала
                                           -- это блокирует окончание выполнения программы до тех пор пока не прочтем все пять
                                    -- список используется как счетчик

{-
thread ничего не хзнает о других threads
    захват ресурсов stdout происходит попеременно и не предсказуемо
отсутствие синхронизации приводит к большим проблемам
можно блокировать разделяемые ресурсы в процессе вычисления одного thread
-}

threadHelloFix :: MVar () -> Chan () -> IO ()
threadHelloFix mutex endFlags = do
    greeting <- getGreeting -- это вычисление не требует доступа к разделяемым ресурсам, можно не тратить время на его ожидание во время блокировки

    takeMVar mutex -- занимаем ресурсы через мьютекс (блокируем)
    putStrLn greeting -- доступ к stdout
    putMVar mutex () -- освобождаем ресурсы (разблокируем)
    
    writeChan endFlags ()

mainChanFix :: IO ()
mainChanFix = do
    hSetBuffering stdout NoBuffering
    let n = 10
    mutex <- newEmptyMVar
    endFlags <- newChan
    mapM_ (\_ -> forkIO $ threadHelloFix mutex endFlags) [1..n]
    putMVar mutex ()
    mapM_ (\_ -> readChan endFlags) [1..n]

{-
Более лаконичное обозначение занятых ресурсов можно организовать с использованием семафоров
https://www.youtube.com/watch?v=x3GwVccWcqs
https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Control-Concurrent-QSem.html
https://hackage.haskell.org/package/base-io-access-0.4.0.0/docs/Access-Control-Concurrent-QSem.html
https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Control-Concurrent-QSemN.html
https://hackage.haskell.org/package/base-io-access-0.4.0.0/docs/Access-Control-Concurrent-QSemN.html
-}


--------------------------------------------------------
{-
Ошибки в процессе выполнения thread могут приводить ко взаимной блокировке (deadlock)


Можно ли синхронизировать конкурентные процессы без блокировки?

Нужны атомарные транзакции разделяемых ресурсов без блокировок
      --------------------                      --------------

Эту задачу решает программная транзакционная память
                    Software Transactional Memory (STM)
    транзакция атомарна - другие thread не могут в нее вмешаться
    если происходят конфликты, то происходит перезапуск thread
        (перезапускается прерывающая транзакция, прерываемая выполняется до конца)
    взаимные блокировки невозможны

Монада STM фактически является действием
    STM нельзя смешивать с IO, т.е. внутри STM не может быть IO, а внутри IO не может быть выполнено STM

Интерфейс:
    atomically :: STM a -> IO a -- атомарно выполняет последовательность STM дейсвтий
                                -- конвертирует STM в IO
    retry :: STM a -- перезапуск STM
    orElse :: STM a -> STM a -> STM a -- композиция STM действий
    check :: Bool -> STM () -- retry по булеву значению
    throwSTM и catchSTM - обработка ошибок

Транзакционные переменные (TVar) работают аналогично MVar, но с оберткой в виде STM
    newTVar :: a -> STM (TVar a)#
    readTVar :: TVar a -> STM a#
    writeTVar :: TVar a -> a -> STM ()


Создадим несколько thread и посчитаем сумму номеров thread:
-}

type Result = TVar (Int, Int)
                  -- 🠕    🠕
                  -- 🠕    Количество законченных транзакций
                  -- Сумма индексов

addToResult :: Result -> Int -> STM () -- это STM действие
addToResult result x = do
    (sum, endCtr) <- readTVar result
    writeTVar result (sum + x, endCtr + 1)

waintForCounter :: Result -> Int -> STM Int
waintForCounter result limit = do
    (sum, endCtr) <- readTVar result
    if endCtr < limit then retry else return sum -- синхронизация thread и перезапуск при конфликтах
    --return sum

mainSTM :: IO ()
mainSTM = do
    let n = 100
    result <- atomically $ newTVar (0, 0) -- atomically для приведения действия STm к IO
    mapM_ (\x -> forkIO $ atomically $ addToResult result x) [1..n]
    sum <- atomically $ waintForCounter result n
    putStrLn $ "Sum = " ++ show sum
