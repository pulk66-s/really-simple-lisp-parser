module Eval (
    eval
) where

import AST 

eval :: Either String (AST, String) -> IO()
eval (Left _)           = print 0
eval (Right (ast, _))   = printEval (evalAst ast)

printEval :: Maybe Float -> IO()
printEval (Just n)  = print n
printEval _         = print 0

evalAst :: AST -> Maybe Float
evalAst (Number n)                  = Just n
evalAst (BinOp op exprs)            = evalBinop op exprs
evalAst _                           = Nothing

extractValues :: [AST] -> Maybe [Float]
extractValues   = mapM evalAst

applyDivision :: [Float] -> Maybe Float
applyDivision []        = Nothing
applyDivision (x:0:_)   = Nothing
applyDivision (x:xs)    = Just (foldl (/) x xs)

applyFunction :: Char -> Maybe [Float] -> Maybe Float
applyFunction '+' (Just l)  = Just (sum l)
applyFunction '*' (Just l)  = Just (product l)
applyFunction '/' (Just l)  = applyDivision l
applyFunction _ _           = Nothing

evalBinop :: Char -> [AST] -> Maybe Float
evalBinop symbol exprs  = applyFunction symbol (extractValues exprs)
