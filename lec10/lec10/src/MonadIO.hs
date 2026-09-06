module MonadIO where

--------------------------------------------------------
{-
Монада IO
Эффект - ввод-вывод (взаимодействие с внешней средой)

Взаимодействие с внешним миром аналогично State:
getCharFromConsole :: RealWorld -> (RealWorld, Char)
                      s         -> (s        , a   )

newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #)) -- magical
! один и тот же RealWorld нельзя использовать два раза (к нему нет доступа)
! единственный способ выполнить действие ввода-вывода — связать его с функцией main

Представитель аналогичен State:
instance Monad IO where
    return x = IO $ \w -> (w, x)
    (>>=) (IO m) k = IO $
        \w -> case m w of (new_w, a) -> unIO (k a) new_w
-- Необходимые гарантии:
! Побочные эффекты происходят строго в заданном порядке
! Побочный эффект каждого действия происходит один раз

Стандартные функции:

getChar :: IO Char -- чтение одного символа
getLine :: IO String -- чтение строки
getContents :: IO String -- чтение всего ввода

putChar :: Char -> IO () -- вывод одного символа
putStr, putStrLn :: String -> IO () -- вывод строки
print :: Show a => a -> IO () -- вывод объекта в виде строки

interact :: (String -> String) -> IO () -- принимает чистую функцию над строками и getLine, а возвращает putStr 


IO маркирует все вычисления происходящие в монаде ввода-вывода
если IO нет то и ввода-вывода нет

Если что-то можно обработать в чистом виде (без IO), то это должно быть обработано в чистом виде

-}

mainBindDo :: IO ()
mainBindDo =
    putStrLn "What is your name?" >>
    getLine >>= \name ->
    putStrLn $ "Nice to meet you, " ++ name ++ "!"
-- mainBindDo
