module Program (
    program
) where

import AST
import Parser
import Data.Char

openPar :: Parser AST
openPar = char' '('

closePar :: Parser AST
closePar    = char' ')'

name :: Parser String
name    = many alpha >>= \r -> case r of
    []  -> empty
    _   -> return r

args :: Parser [String]
args    = many (name >>= \n -> spaces >> return n)

defFunc :: Parser AST
defFunc = string' "defun" >> spaces >>
    name >>= \n ->
    spaces >> openPar >>
    args >>= \a -> closePar >>
    spaces >>
    expression >>= \body -> 
    return (DefFunc n a body)

callFunc :: Parser AST
callFunc = name >>= \n ->
    many (spaces >> expression >>= \e -> return e) >>= \par ->
    return (CallFunc n par)

expression :: Parser AST
expression  = (openPar
    >> spaces >> (binop <|> defFunc <|> callFunc) >>= \expr -> 
    spaces >> closePar >> return expr) <|> number <|> (name >>= \n -> return (Keyword n))

isOperator :: Char -> Bool
isOperator '+' = True
isOperator '-' = True
isOperator '*' = True
isOperator '/' = True
isOperator _   = False

operator :: Parser Char
operator    = satisfy isOperator

binop :: Parser AST
binop   = operator 
        >>= \o -> many (spaces >> expression)
        >>= \exprs -> return (BinOp o exprs)

space :: Parser Char
space   = char ' '

spaces :: Parser AST
spaces  = many space >> return (Keyword " ")

number :: Parser AST
number  = many (satisfy isDigit) >>= \r -> case r of
    []  -> empty
    _   -> return (Number (read r))

program :: Parser [AST]
program = many (expression >>= \e -> spaces >> return e)
