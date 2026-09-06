module MonadMaybe where

--------------------------------------------------------
{-
Монада Maybe
Эффект - возможно отсутствующее значение (конструктор Nothing)


Представитель Maybe для Monad:

instance Monad Maybe where
    (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    (Just x) >>= k = k x
    Nothing >>= _ = Nothing

    (>>) :: Maybe a -> Maybe b -> Maybe b
    (Just _) >> m = m
    Nothing >> _ = Nothing
    
    return :: a -> Maybe a
    return = Just

Пример: поиск родственников по генеалогическому дереву
    Список пар [(ребенок, родитель)]
    Возможность отсутствия родителя
-}

type Name = String
type ParentsTable = [(Name,Name)]

fathers, mothers :: ParentsTable
fathers = [("Bill","John"),("Ann","John"), ("John","Piter")]
mothers = [("Bill","Jane"),("Ann","Jane"), ("John","Alice"),("Jane","Dorothy"), ("Alice","Mary")]

{-
стрелки Клейсли для Maybe
           :: a    -> m     b
-}
getM, getF :: Name -> Maybe Name
getM person = lookup person mothers -- :t lookup - поиск в ассоциативном списке
getF person = lookup person fathers
{-
ищем прабабушку (getM) по материнской линии отца (getF):
getF "Bill" >>= getM >>= getM
do {f <- getF "Bill"; m <- getM f; getM m}

поиск бабушек:
-}
granmas :: Name -> Maybe (Name, Name)
granmas person = do
    m <- getM person
    gmm <- getM m
    f <- getF person
    gmf <- getM f
    return (gmm, gmf)
{-
granmas "Ann"
granmas "John"

Без использования монад на каждом этапе пришлось бы проверять не является ли результат Nothing
    монады передают эффект отсутствующего значения автоматически

Что если хотим сопоставлять с образцом имя родителя в gramnas?
Могут возникнуть ситуации с неудачей
Нужно их обрабатывать





--------------------------------------------------------
Класс типов MonadFail предназначен для обработки неудачного сопоставления с образцом слева от (<-) в do-нотации:

('A':x) <- getM p

если сопоставление с образцом ('A':x) результата выполнения getM p будет неудачным, то будет вызван fail


в древности fail относится к классу типов Monad, но в текущей версии языка от относится к MonadFail
    т.к. для монад без семантики ошибок fail определялся как error и от этого избавились



class Monad m => MonadFail m where
    fail :: String -> m a -- маркируется ошибка в монадическом значении


instance MonadFail Maybe where
    fail _ = Nothing


do {3 <- Just 5; return 'A'}
                                    Just 5 >>= \x -> case x of
                                        3 -> return 'A'
                                        _ -> fail "fail Maybe"
do {3 <- Identity 5; return 'A'}
                                    Identity 5 >>= \x -> case x of
                                        3 -> return 'A'
                                        _ -> fail "fail Indentity"
No instance for `MonadFail Identity' ... -- не компилируется на уровне типов


Закон класса типов MonadFail (связывает классы типов Monad и MonadFail):
fail s >>= k ≡ fail s

:t fail "Oh!" >>= granmas
fail "Oh!" >>= granmas
fail "Oh!" :: Maybe (Name, Name)

-}
