module MonadRWS where

import qualified Control.Monad.RWS as RWS

{-
Монада RWS - объединяет три концепции монад Reader, Writer и State

Reader r  - внешнее окружение
Writer w  - лог
State s   - мутабельное состояние


newtype RWST r w s m a = RWST { runRWST :: r -> s -> m (a, s, w) }

rws :: (r -> s -> (a, s, w)) -> RWS r w s a

runRWS :: RWS r w s a -> r -> s -> (a, s, w)


https://hackage-content.haskell.org/package/transformers-0.6.3.0/docs/src/Control.Monad.Trans.RWS.Lazy.html

instance (Monoid w, Monad m) => Monad (RWST r w s m) where
    return a = RWST $ \ _ s -> return (a, s, mempty)

    m >>= k  = RWST $ \ r s -> do
        (a, s', w)  <- runRWST m r s
        (b, s'',w') <- runRWST (k a) r s'
        return (b, s'', w `mappend` w')


Пример:
counter = RWS.RWST $ \() s -> return (s, s+1, ["tick " ++ show s])
:t
RWS.runRWS counter () 0
RWS.runRWS (counter >>= \_ -> counter) () 0
-}



{-
Пример использования монады RWS
- Используем окружение (Reader) для хранения коэффициента умножения.
- Накопим лог (Writer) в виде списка строк.
- Управляем состоянием (State) — счетчиком операций.

Типы: RWS (Reader) (Writer) (State) Результат
    В данном случае:
        Reader: Int (коэффициент умножения)
        Writer: [String] (лог)
        State: Int (счетчик операций)
        Результат: Int (результат вычисления)
-}

rwsExample :: RWS.RWS Int [String] Int Int
rwsExample =
    RWS.ask >>= \coefficient ->                                 -- коэффициент из Reader
    RWS.get >>= \counter ->                                     -- текущее состояние (счетчик)
    RWS.put (counter + 1) >>                                    -- изменение состояния
    RWS.tell ["Increas counter to " ++ show (counter + 1)] >>   -- логирование действия
    return (coefficient * counter)                              -- результат (коэффициент * счетчик)

(resultRWS, logsRWS, finalStateRWS) = RWS.runRWS rwsExample 5 1

{-
ask >>= \coefficient -> 
    ask получает значение из Reader (коэффициент 5)
    Результат передается в лямбда-функцию \coefficient -> 

get >>= \counter -> 
    get получает текущее состояние (счетчик 1)
    Результат передается в лямбда-функцию \counter -> 

put (counter + 1) >> 
    put обновляет состояние, увеличивая счетчик на 1
    Оператор >> игнорирует результат put и переходит к следующему действию

tell ["Увеличили счетчик до " ++ show (counter + 1)] >>
    tell добавляет запись в лог
    Оператор >> игнорирует результат tell и переходит к следующему действию

return (coefficient * counter)
    Возвращает результат вычисления (коэффициент умноженный на счетчик)
-}

mainRWS = do
    putStrLn $ "Результат: " ++ show resultRWS
    putStrLn $ "Логи: " ++ show logsRWS
    putStrLn $ "Финальное состояние: " ++ show finalStateRWS

{-
Начальное состояние счетчика — 1
Коэффициент из Reader — 5
Результат вычисления: 5 * 1 = 1
Счетчик увеличивается до 2
В лог добавляется запись о том, что счетчик увеличен
-}
