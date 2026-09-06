{-# LANGUAGE OverloadedStrings #-}

module Palindrome ( palindrome ) where

{-
Реализуем функцию, которая определяет является введенная строка полиндромом или нет
-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Char ( isSpace, isPunctuation )

stripWhiteSpace :: T.Text -> T.Text
stripWhiteSpace text = T.filter (not . isSpace) text

stripPunctuation :: T.Text -> T.Text
stripPunctuation text = T.filter (not . isPunctuation) text

preProcess :: T.Text -> T.Text
preProcess = stripWhiteSpace . stripPunctuation . T.toLower

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText
    where cleanText = preProcess text


palindrome :: IO ()
palindrome = do
    TIO.putStrLn "Enter a word and I'll let you know if it's a palindrome!"
    text <- TIO.getLine
    let response = if isPalindrome text
                   then "it is!"
                   else "it's not!"
    TIO.putStrLn response
