module Macro (
    evalMacro
) where

import AST

type Environment = [(String, [String], AST)]

evalMacro :: Either String ([AST], String)
evalMacro (Right (exprs, r))    = Right (applyEnv exprs (generateEnv exprs))
evalMacro other                 = other

generateEnv :: [AST] -> ([AST], Environment)
generateEnv exprs   = (filterDefFunc exprs, getFunctionsEnv exprs)

filterDefFunc :: [AST] -> [AST]
filterDefFunc   = filter (\(DefFunc {}) -> True)

getFunctionsEnv :: [AST] -> Environment
getFunctionsEnv []                          = []
getFunctionsEnv (DefFunc name args body:xs) = (name, args, body) : getFunctionsEnv xs
getFunctionsEnv (x:xs)                      = getFunctionsEnv xs

applyEnv :: [AST] -> Environment -> ([AST], String)
applyEnv [] _       = []
applyEnv (x:xs) env = replaceCallFunc env x : applyEnv xs env

replaceCallFunc :: Environment -> AST -> AST
replaceCallFunc env (BinOp op exprs)    = BinOp op (map (replaceCallFunc env) exprs)
replaceCallFunc env (DefFunc n a body)  = DefFunc n a (replaceCallFunc body)
replaceCallFunc env (CallFunc n params) = CallFunc n (map (replaceCallFunc env) params)
replaceCallFunc _ other                 = other

containsFunction :: Environment -> String -> Bool
containsFunction _ "" = True
containsFunction _ _ = False
