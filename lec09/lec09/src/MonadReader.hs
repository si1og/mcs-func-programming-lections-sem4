module MonadReader where

import Control.Monad.Trans.Reader ( Reader, runReader, reader, ask, asks, local )
import Data.Maybe ( fromMaybe )

--------------------------------------------------------
{-
Монада Reader
Эффект - чтение из разделяемого внешнего окружения (Environment)



Частично примененная стрелка, 
    аналог представителей функторов - композиция;
    апприкативных функторов (pure) - const, ap - передача одного аргумента в два вычисления.

instance Monad ((->) r) where
    return :: a -> (r -> a)
    return x = \_ -> x -- игнорирование окружения

    (>>=) :: (r -> a) -> (a -> (r -> b)) -> (r -> b)
    m >>= k = \e -> k (m e) e -- передает окружение в два вычисления (и в монаду и в стрелку Клейсли)



Тип Reader:
newtype Reader r a = Reader { runReader :: r -> a }
Интерфейсы Reader:
reader :: (r -> a) -> Reader r a -- преобразует функцию из окружения в вычисление в монаде
runReader :: Reader r a -> r -> a


                        частично примененная функциональная стрелка
                        🠗        внешнее окружение
                        🠗        🠗
runReader (reader (\x -> x * 2)) 3
🠕          🠕
🠕          упаковка стрелки в контекст Reader
запуск вычисления в контексте Reader



Представитель является простой переупаковкой в Reader:
instance Monad (Reader r) where
    return x = reader $ \_ -> x
    m >>= k = reader $ \e -> let v = runReader m e
                             in runReader (k v) e



do {a <- (^2); b <- (*5); return (a + b)} $ 3                                   -- константное окружение к которому обращается в течении всего вычисления
runReader (reader (^2) >>= (\x -> reader (*5) >>= (\y -> return (x + y)))) 3

runReader (
           reader (^2) >>= (\x  -- х связывается со стрелкой (^2)
        -> reader (*5) >>= (\y  -- y связывается со стрелкой (*5)
        -> return (x + y)))
           ) 3                  -- окружение пережается в монаду после запуска через runReader

-}

simpleReader :: Show r => Reader r String -- (r -> String)
simpleReader = reader (\e -> "Environment is " ++ show e)
{-
runReader simpleReader True
runReader simpleReader [1,2,3]



Стандартный интерфейс Reader:
    ask     - возвращает (получает) текущее окружение
    asks    - возвращает результат выполнения функции над окружением
    local   - локально модифицирует окружение
-}
type User = String
type Password = String
type UsersTable = [(User,Password)]

pwds :: UsersTable
pwds = [("Bill","123"),("Ann","qwerty"),("John","2sRq8P")] -- ассоциативный список

-- ask :: Reader r a -- возвращает окружение
firstUser :: Reader UsersTable User -- возвращает имя первого пользователя в списке
firstUser = do
    e <- ask -- ask = reader id -- получение текущего окружения
             -- :t (runReader ask)
             -- (runReader ask) :: a -> a
    let name = fst (head e)
    return name
-- runReader firstUser pwds
-- runReader firstUser []

-- asks :: (r -> a) -> Reader r a -- возвращает результат выполнения функции над окружением
getPwdLen :: User -> Reader UsersTable Int -- определяет длину пароля
getPwdLen person = do
    mbPwd <- asks $ lookup person -- lookup - в ассоциативном списке по ключу ищет Maybe значение
          -- asks - получение конкретных полей из окружения 
    let mbLen = fmap length mbPwd -- костыли с fmap -- лучше было бы использовать Reader и Maybe как монады оновременно
    let len = fromMaybe (-1) mbLen -- fromMaybe - распаковка Maybe
    return len
-- runReader (getPwdLen "Ann") pwds
-- runReader (getPwdLen "Ann") []

-- local :: (r -> r) -> Reader r a -> Reader r a -- позволяет локально модифицировать окружение
usersCount :: Reader UsersTable Int
usersCount = asks length -- вспомогательная функция

localTest :: Reader UsersTable (Int,Int,Int)
localTest = do
    count1 <- usersCount 
    count2 <- local (("Mike","1"):) usersCount -- изменение окружения, только в этой строчке
    count3 <- usersCount -- здесь окружение не измененное
    return (count1, count2, count3)
-- runReader localTest pwds
-- runReader localTest []
