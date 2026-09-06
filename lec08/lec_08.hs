----------------------------------------------
-- Лекция 08 DO нотация. Ввод-вывод. Монады --
----------------------------------------------

module Main where
{-
Стандартная точка входа в программу - модуль Main, функция main
-}

import System.Environment (getArgs)
import Control.Monad (replicateM)
import System.IO (openFile, IOMode( ReadMode, WriteMode ), hClose, 
                    hGetLine, hPutStrLn, hIsEOF)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

--------------------------------------------------------
{- ------------------------- 1 -------------------------
Ввод-вывод
Поток ввода-вывода представим в виде лениво вычисляемого списка символов
STDIN (стандартный поток ввода) последовательно передаёт пользовательский ввод программе до тех пор, пока не достигнет его конца
! Заранее неизвестно, где находится этот конец (теоретически, его может вообще не быть).



Возьмем простой код, который берет пользовательский ввод и выводит его не экран:
(Доступ к аргументам командной строки)

main :: IO ()
main = do
    args <- getArgs
    mapM_ putStrLn args

В do-нотации используются функции следующих типов:
mapM_ :: (Foldable t, Monad m) => (a      -> m  b) -> t  a        -> m  ()
getArgs ::                                            IO [String]
putStrLn ::                        String -> IO ()
                                                                     IO () -- то что должно получиться на выходе (из имеющихся типов)

-- ghc lec_08.hs
-- ./lec_08 2 3 4
-}

main :: IO ()
main = do
    args <- getArgs
    mapM_ putStrLn args

{- ------------------------- 2 -------------------------
Приведем тип входного потока к Int, для упраления пользовательским вводом:
read :: String -> Int

Функция linesToRead будет парсить число строк, которое хочет ввести пользователь

main :: IO ()
main = do
    args <- getArgs
    let linesToRead = if length args > 0
                      then (read (head args)) :: Int
                      else 0 :: Int
    print linesToRead

-}



{- ------------------------- 3 -------------------------
Используем linesToRead как аргумент при запросе данных от пользователя
(считываем строки в заданном количестве)

                               linesToRead (количество строк для ввода)
                               🠗
replicateM :: Applicative m => Int -> m  a      -> m [a]
getLine ::                            IO String


main :: IO ()
main = do
    args <- getArgs
    let linesToRead = if length args > 0
                      then read (head args)
                      else 0 :: Int
    numbers <- replicateM linesToRead getLine
    print "Here is the sum: "

-}



{- ------------------------- 4 -------------------------
приводим типы входных строк к Int и применяем чистую функцию к списку значений

main :: IO ()
main = do
    args <- getArgs
    let linesToRead = if length args > 0
                      then read (head args)
                      else 0 :: Int
    numbers <- replicateM linesToRead getLine
    let ints = map read numbers :: [Int] -- преобразование типов
    print ("Here is the sum: " ++ show (sum ints))

-}



{-
mapM - Принимает на вход действие ввода-вывода и обычный список,
    выполняет действие на каждом элементе списка и возвращает
    список в контексте IO
mapM_ - Работает как mapM, но отбрасывает результат (обратите внимание на _)
replicateM - Принимает на вход действие ввода-вывода и целое число n,
    повторяет действие n раз и возвращает результаты в виде
    списка в контексте IO
replicateM_ - Работает как replicateM, но отбрасывает результат
-}



{- ------------------------- 5 -------------------------
Ленивый ввод-вывод

Если пользователь заранее не знает сколько он хочет ввести значений, то нужно дать ему возможность вводить значения бесконечно

getContents :: IO String -- действие getContents позволяет работать с потоком ввода STDIN как со списком символов

print :: Show a => a -> IO () -- выводит любой тип с представителем Show в поток IO


main :: IO ()
main = do
    userInput <- getContents
    mapM_ print userInput

-}



{- ------------------------- 6 -------------------------

Допишем суммирование произвольного числа элементов 

main :: IO ()
main = do
    userInput <- getContents
    let numbers = toInts userInput
    print (sum numbers)

-}

toInts :: String -> [Int] -- преобразования Char в Int
toInts = map read . lines





--------------------------------------------------------
{-
Работа с внешними фалами
Открытие, закрытие, чтение, запись, ленивые и энергичные подходы


Как открыть и закрыть файл?

:t openFile
    openFile :: FilePath -> IOMode -> IO Handle
:i FilePath
    FilePath = String
:i IOMode
    data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
IO Handle - файловый дескриптор (идентификатор потока ввода-вывода)


-- ------------------------- 7 -------------------------
Функция main открывает и закрывает файл если он есть

main :: IO ()
main = do
    myFile <- openFile "hello.txt" ReadMode -- открыли файл
    hClose myFile                           -- закрыли файл
    putStrLn "done"                         -- выводим на экран сообщение об успешном выполнении задачи

Что будет если файл НЕ существует в директории?
Что будет если файл существует в директории?

-}



{- ------------------------- 8 -------------------------

Как читать из файла и записывать в файл?


Чтение строки из файла:

                            Строка из файла в обертке IO
                            🠗
    hGetLine :: Handle -> IO String
                🠕
                Файловый дескриптор без IO


Запись строки в файл:
                       Строка для записи в файл
                       🠗
hPutStrLn :: Handle -> String -> IO ()
             🠕
             Файловый дескриптор


main :: IO ()
main = do
    helloFile <- openFile "hello.txt" ReadMode
    firstLine <- hGetLine helloFile                 -- (hGetLine) передаем дескриптор в качестве аргумента (getLine - частный случай hGetLine stdout)
    putStrLn firstLine
    secondLine <- hGetLine helloFile                -- (hGetLine) передаем дескриптор в качестве аргумента
    goodbyeFile <- openFile "goodbye.txt" WriteMode
    hPutStrLn goodbyeFile secondLine                -- (hPutStrLn) передаем дескриптор в качестве аргумента (putStrLn - частный случай hPutStrLn)
    hClose goodbyeFile
    hClose helloFile
    putStrLn "done"

-}



{- ------------------------- 9 -------------------------
Наличие информации в файле не гарантированно (это внешний мир, у него есть эффекты, он не предсказуем)
Не всегда известно когда файл закончится

hIsEOF :: Handle -> IO Bool


main :: IO ()
main = do
    helloFile <- openFile "hello_.txt" ReadMode -- файл с одной строкой
    hasLine <- hIsEOF helloFile -- проверка файла на не пустоту перед чтением из него
    firstLine <- if not hasLine
                 then hGetLine helloFile
                 else return "empty file"
    putStrLn firstLine
    hasSecondLine <- hIsEOF helloFile -- второй строки нет
    secondLine <- if not hasSecondLine
                  then hGetLine helloFile
                  else return "second line is empty"
    putStrLn secondLine
    putStrLn "done"

-}



{- ------------------------- 10 -------------------------
Ручной контроль файловых дескрипторов требует написания однообразного кода и засоряет код
Хотелось бы уметь работать с файлами без прямого взаимодействия с файловыми дескрипторами

Стандартные функции чтения, записи и добавления бе дескрипторов:
readFile :: FilePath -> IO String
writeFile :: FilePath -> String -> IO ()
appendFile :: FilePath -> String -> IO ()


Реализуем программу, которая:
    - принимает имя файла в качестве аргумента
    - подсчитывает количество символов, слов и строк в файле
    - выводит данные пользователю на экран
    - добавляет данные в конец файла stats.dat


main :: IO () -- **
main = do
    args <- getArgs
    let fileName = head args
    input <- readFile fileName
    let summary = (countText . getCounts) input
    appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"])
    putStrLn summary

-- lec_08 hello.txt
-}

getCounts :: String -> (Int, Int, Int)
getCounts input = (charCount, wordCount, lineCount)
    where charCount = length input
          wordCount = (length . words) input
          lineCount = (length . lines) input

countText :: (Int, Int, Int) -> String
countText (cc, wc, lc) = unwords ["chars: ", show cc, " words: ", show wc, "lines: ", show lc]



{- ------------------------- 11 -------------------------

Что произойдет если передать в программу файл сохраняющий статистику?

-- lec_08 stats.dat

Почему так происходит?

readFile не закрывает файловый дескриптор
Рассмотрим библиотечную реализацию readFile:

readFile :: FilePath -> OI String
readFile name = do
    inputFile <- openFile name ReadMode
    hGetContents inputFile

! в реализации отсутствует команда на закрытие дескриптора !

Попробуем переписать main из пункта 9 с явным закрытием файлового дескриптора:

закрыть файл можно в двух местах (рассмотрим оба):

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    file <- openFile fileName ReadMode  -- открыли файл (энергично)
    input <- readFile fileName          -- ленивость (связывается, но не выполняется)
    --hClose file                         -- закрыли файл (энергично, потому что ждать нечего)
    let summary = (countText . getCounts) input -- ленивость (здесь input все еще не выполнился)
    appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"]) -- ленивость требует выполнения отложенных summary и input, но файл уже закрыт
    hClose file -- закроет файл в процессе записи
    putStrLn summary

Как в этих случаях будет вести себя:
-- lec_08 stats.dat
-}



{- ------------------------- 12 -------------------------
Ленивость вычислений мешаем обрабатывать файлы!

Перепишем main из пункта 11 с использованием форсированных вычислений:


main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    file <- openFile fileName ReadMode  -- открыли файл (энергично)
    input <- readFile fileName          -- ленивость (связывается, но не выполняется)
    let summary = (countText . getCounts) input -- ленивость (здесь input все еще не выполнился)
    putStrLn summary -- форсирование вычисления summary
    hClose file      -- закрыли файл (энергично, что ждать нечего)
    appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"])
    putStrLn "done"

Проверим что все работает
Форсирование вычислений через вывод на экран, не очень элегантное решение
Можно сделать лучше
-}



{- ------------------------- 13 -------------------------
Строгие (неленивые) типы (Data.Text) и ввод-вывод

Заменим все типы String в main из пункта 11 на text
Так же приведем IO к нужному типу Text


main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    input <- TIO.readFile fileName
    let summary = (countTextT . getCountsT) input
    TIO.appendFile "stats.dat" (mconcat [(T.pack fileName), " ", summary, "\n"])
    TIO.putStrLn summary


! Не компилируется
Есть некоторые проблемы с типами, можно решить из через расширение:

{-# LANGUAGE OverloadedStrings #-} - расширение, которое делает строковые литералы перегруженными
Т.е. все у чего есть представитель IsString из Data.String будет автоматически приведено к нужному типу из Text, ByteString, FilePath, ...

-}

getCountsT :: T.Text -> (Int, Int, Int)
getCountsT input = (charCount, wordCount, lineCount)
    where charCount = T.length input
          wordCount = (length . T.words) input
          lineCount = (length . T.lines) input

countTextT :: (Int, Int, Int) -> T.Text
countTextT (cc, wc, lc) = T.pack (unwords ["chars: ", show cc, " words: ", show wc, "lines: ", show lc])





--------------------------------------------------------
{-
Тип String является списком Char, что приводит к не эффективностям при использовании памяти и скорости работы кода

Тип Text из Data.Text работает с текстом как с упакованным массивом байтов, 
    в нем отсутствуют дополнительные указатели, поддерживает Unicode,
    не использует ленивые вычисления
    (есть реализация Data.Text.Lazy с возможностью использования ленивого подхода и сохранения структуры данных)



Перевод из String в Text и обратно:
    T.pack :: String -> T.Text
    T.unpack :: T.Text -> String

fisrstWord :: String
fisrstWord = "firstWord"

secondWord :: T.Text
secondWord = T.pack fisrstWord

thirdWord :: String
thirdWord = T.unpack secondWord



Простая обработка текста:
    T.lines :: T.Text -> [T.Text]
    T.unlines :: [T.Text] -> T.Text
    T.words :: T.Text -> [T.Text]
    T.unwords :: [T.Text] -> T.Text
    T.splitOn :: T.Text -> T.Text -> [T.Text]
    T.intercalate :: T.Text -> [T.Text] -> T.Text

sampleInput :: T.Text    
sampleInput = "this\nis\nsome\ntext"
-- T.lines sampleInput
-- T.unlines (T.lines sampleInput)


someInput :: T.Text    
someInput = "this\nis\nsome other\ttext"
-- T.lines someInput
-- T.words someInput
-- T.unwords (T.words someInput)


breakText :: T.Text
breakText = "simple"

newText :: T.Text
newText = "This is simple text"
-- T.splitOn breakText newText
-- T.intercalate breakText (T.splitOn breakText newText)


combinedTextMonoid :: T.Text
combinedTextMonoid = mconcat ["some"," ","text"]

combinedTextSemigroup :: T.Text
combinedTextSemigroup = "some" <> " " <> "text"



Функция просматривает весь текст находит в нем все вхождения участка строки "धर्म" и обрамляет их скобками { }:

dharma :: T.Text
dharma = "धर्म" -- chcp 65001 -- unicode

bgText :: T.Text
bgText = "श्रेयान्स्वधर्मो विगुणः परधर्मात्स्वनुष्ठितात्।स्वधर्मे निधनं श्रेयः परधर्मो भयावहः"

highlight :: T.Text -> T.Text -> T.Text
highlight query fullText = T.intercalate highlighted pieces
    where pieces = T.splitOn query fullText
          highlighted = mconcat [" {", query, "} "]

main :: IO ()
main = do
    TIO.putStrLn (highlight dharma bgText)

-}





--------------------------------------------------------
{-
Rand
Hello
Pizza
Palindrome
Searcher
-}
