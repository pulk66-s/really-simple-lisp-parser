module AST (
    AST(..),
    Parser(..),
    Alternative(..),
    printResult,
    runParser
) where

import Control.Applicative

data AST =  Keyword String              |
            Symbol Char                 |
            Number Float                |
            BinOp Char [AST]            |
            DefFunc String [String] AST |
            CallFunc String [AST]
            deriving (Show, Eq)

newtype Parser a = Parser {
    run :: String -> Either String (a, String)
}

instance Functor Parser where
    fmap f (Parser p) = Parser $ \s -> case p s of
        Left err -> Left err
        Right (x, s') -> Right (f x, s')

instance Applicative Parser where
    pure a                  = Parser func
        where
            func str         = return (a, str)
    Parser p1 <*> Parser p2 = Parser func
        where
            func str = do
                (f, str') <- p1 str
                (a, str'') <- p2 str'
                return (f a, str'')

instance Monad Parser where
    Parser p >>= f  = Parser func
        where
            func str = do
                (a, str') <- p str
                let (Parser b) = f a
                b str'

instance Alternative Parser where
    empty                   = Parser $ \_ -> Left "empty"
    Parser p1 <|> Parser p2 = Parser func
        where
            func str = case p1 str of
                Left _  -> p2 str
                other   -> other

printAstList :: [AST] -> IO()
printAstList = foldr ((>>) . printAst) (return ())

printAst :: AST -> IO()
printAst (Keyword str)      = putStrLn ("KeyWord: (" ++ str ++ ")")
printAst (Symbol c)         = putStrLn ("Symbol: " ++ [c])
printAst (Number n)         = putStr "Number: " >> print n
printAst (BinOp op exprs)   = putStrLn ("BinOp: {Operator: " ++ [op]) >> printAstList exprs >> putStrLn "}"
printAst (DefFunc n a b)    = putStrLn ("DefFunc: {Name: " ++ n ++ ", Args: ") >> mapM print a >> putStrLn "}" >> printAst b
printAst (CallFunc n a)     = putStrLn ("CallFunc: {Name: " ++ n ++ ", Args: ") >> mapM print a >> putStrLn "}"

printResult :: Either String ([AST], String) -> IO()
printResult (Left r)            = putStr "Err " >> putStrLn r
printResult (Right (ast, rest)) = putStr "Success "
    >> mapM printAst ast
    >> putStrLn ("Rest (" ++ rest ++ ")")

runParser :: Parser [AST] -> String -> Either String ([AST], String)
runParser (Parser f) = f
