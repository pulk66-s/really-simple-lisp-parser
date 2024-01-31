module Parser (
    char,
    char',
    string',
    satisfy,
    alpha,
    nothing
) where

import AST
import Data.Char

satisfy :: (Char -> Bool) -> Parser Char
satisfy f   = Parser func
    where
        func []                 = Left "empty"
        func (x:xs) | f x       = Right (x, xs)
                    | otherwise = Left ("Don't match got: " ++ [x] ++ " rest: (" ++ xs ++ ")")

char :: Char -> Parser Char
char c  = satisfy (== c)

char' :: Char -> Parser AST
char' c = char c >>= \r -> return (Symbol r)

string :: String -> Parser String
string  = mapM char

string' :: String -> Parser AST
string' str = string str >>= \r -> return (Keyword r)

alpha :: Parser Char
alpha   = satisfy isAlpha

nothing :: Parser AST
nothing = Parser func
    where
        func l = Right (Number 1, l)
