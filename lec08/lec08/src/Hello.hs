module Hello ( hello, echo, helloName) where

{-
Реализуем функцию, которая 
    запрашивает имя пользователя
    приветствует пользователя по имени
-}

helloPerson :: String -> String
helloPerson name = "Hello, " ++ name ++ "!"

{-
hello :: IO ()
putStrLn :: String -> IO ()
getLine :: IO String
    не являются функциями в математическом смысле (функции должны возвращать значения)
    это действия
-}

hello :: IO ()
hello = do
    putStrLn "Hello! What's your name?"
    name <- getLine
    let statement = helloPerson name
    putStrLn statement



--------------------------------------------------------
-- Searcher.hs
--------------------------------------------------------
{-
Функция echo считывает пользовательский ввод и сразу выводит на экран

Есть стандартные функции:
getLine :: IO String
putStrLn :: String -> IO ()

Если их скомбинировать то получим тип:
:: IO String -> (String -> IO ()) -> IO ()
-}
echo :: IO ()
echo = getLine >>= putStrLn

{-
Рассахаривание do нотации в функции hello:
-}
askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

helloName :: IO ()
helloName = askForName >>
    getLine >>=
        (\name -> return (nameStatement name)) >>=
            putStrLn
