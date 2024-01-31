module LispInterpreter
    ( launchLispInterpreter
    ) where


understandCommand :: String -> IO()
understandCommand "exit"    = putStrLn "Bye!"
understandCommand cmd       = putStrLn ("You said: " ++ cmd) >> ttyLoop

ttyLoop :: IO()
ttyLoop = getLine >>= understandCommand

launchLispInterpreter :: IO()
launchLispInterpreter = ttyLoop
