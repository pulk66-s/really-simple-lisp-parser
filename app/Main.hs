module Main (main) where

import AST
import Program
import Eval
import Macro

main :: IO()
main = printResult (runParser program "(defun double (x) (* x 2))")
