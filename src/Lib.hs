{-# Language OverloadedStrings #-}
module Lib (someFunc) where

import Data.DoList (DoList, fromList, toList)
import Control.Monad
import Text.Parsec
import Data.Monoid

someFunc :: IO ()
someFunc = do
    forM_ testPhoneNumbers $ \number -> do
        print $ parse phoneNumber "Number: " number

phoneNumber :: Parsec String u String
phoneNumber = plusNum <|> num

-- номера начинающиеся на +7 или 8
plusNum :: Parsec String u String
plusNum = do
    string "+7" <|> string "8"
    code    <- readCode
    number  <- readNumber
    pure $ "+7" <> code <> number

-- остальные номера
num :: Parsec String u String
num = do
    number <- readNumber
    pure $ "+7(343)" <> number


readCode :: Parsec String u String
readCode = do
    optional $ char '('
    code    <- count 3 digit
    optional $ char ')'
    pure $ "(" <> code <> ")"


readNumber :: Parsec String u String
readNumber = do
    digs1 <- count 3 digit
    optional $ char '-'
    digs2 <- count 2 digit
    optional $ char '-'
    digs3 <- count 2 digit
    pure $ digs1 <> "-" <> digs2 <> "-" <> digs3


testPhoneNumbers :: [String]
testPhoneNumbers = toList $ do
    "89006508005"
    "9119990868"
    "+7(999)710-90-88"
    "89015572218"
    "89250475333"
    "77717793929"
    "+74973273032"
    "8921134454454454"
    "8900650800"
    "+791111123"
    "2492996"
