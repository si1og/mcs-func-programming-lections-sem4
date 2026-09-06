module Searcher where

{-
Задача: поиск по двум базам данных, для управления пользовательскими данными
    Пользователь определён как уникальный GamerId (Int) и 
    Уникальный UserName (String), для ассоциации пользователя с данными в профиле
    Из-за устаревшей зависимости от имени пользователя как идентификатора для поиска
        пользовательских данных новых пользователей придётся искать имя пользователя
        с помощью GamerId, а затем применять UserName для поиска данных
        в пользовательской учётной записи
-}

import qualified Data.Map as Map -- Доп. зависимость: containers

type UserName = String
type GamerId = Int
type PlayerCredits = Int



-- БД UserName по GamerId
userNameDB :: Map.Map GamerId UserName
userNameDB = Map.fromList [(1,"nYarlathoTep")
    ,(2,"KINGinYELLOW")
    ,(3,"dagon1997")
    ,(4,"rcarter1919")
    ,(5,"xCTHULHUx")
    ,(6,"yogSOThoth")]

-- БД PlayerCredits по UserName
creditsDB :: Map.Map UserName PlayerCredits
creditsDB = Map.fromList [("nYarlathoTep",2000)
    ,("KINGinYELLOW",15000)
    ,("dagon1997",300)
    ,("rcarter1919",12)
    ,("xCTHULHUx",50000)
    ,("yogSOThoth",150000)]

{-
Напишем функцию, которая 
    ищет PlayerCredits по GamerID
    учитывает возможность отсутствия значения
Такая функция будет иметь тип :: GamerId -> Maybe PlayerCredits

Функция из библиотеки:
                       Ключ
                       🠗
Map.lookup :: Ord k => k -> Map.Map k a -> Maybe a
                            🠕              🠕
                            Словарь        Возможный результат

Аналогичное решение используем для функции поиска PlayerCredits по UserName для второй БД
-}

lookupUserName :: GamerId -> Maybe UserName
lookupUserName gamerId = Map.lookup gamerId userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits username = Map.lookup username creditsDB 

{-
Эти функции нужно соединить для поиска сразу через две базы. При это нужно сохранить эффекты

Т.е. по GamerId нужно получить Maybe PlayerCredits

Результат работы lookupUserName от GamerId нужно передать в lookupCredits
Такая функция будет иметь тип :: Maybe UserName -> (UserName -> Maybe PlayerCredits) -> Maybe PlayerCredits

Ни Functor ни Applicative здесь не помогут, нужна дополнительная обертка:
-}
altLookupCredits :: Maybe UserName -> Maybe PlayerCredits
altLookupCredits Nothing = Nothing
altLookupCredits (Just username) = lookupCredits username
{-
Итоговая функция для поиска через две базы:
-}
creditsFromId' :: GamerId -> Maybe PlayerCredits
creditsFromId' gamerId = altLookupCredits (lookupUserName gamerId)
--  creditsFromId' 1
--  creditsFromId' 100

{-
Расширение и модификация такого кода трудозатртно


Есть стандартный подход для решения таких задач:
Монадический баинд (bind)
(>>=) :: Monad m => m a -> (a -> m b) -> m b

::            Maybe UserName -> (UserName -> Maybe PlayerCredits) -> Maybe PlayerCredits
:: Monad m => m     a        -> (a        -> m     b)             -> m     b

Избавимся от лишнего кода altLookupCredits и модифицируем creditsFromId'
creditsFromId без дополнительной обертки:
-}
creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId gamerId = lookupUserName gamerId >>= lookupCredits


{-
Попробуем расширить поиск (добавим еще один уровень вложенности) через еще одну базу с аналогичной идеей:
-}
type WillCoId = Int
-- Третья БД:
gamerIdDB :: Map.Map WillCoId GamerId
gamerIdDB = Map.fromList [(1001,1), (1002,2), (1003,3), (1004,4), (1005,5), (1006,6)]

lookupGamerId :: WillCoId -> Maybe GamerId
lookupGamerId willCoId = Map.lookup willCoId gamerIdDB

creditsFromWCId :: WillCoId -> Maybe PlayerCredits
creditsFromWCId willCoId = lookupGamerId willCoId >>= lookupUserName >>= lookupCredits
-- creditsFromWCId 1001
-- creditsFromWCId 100
