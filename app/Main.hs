module Main where

import Evaluator.Repl (runFile, runRepl)
import System.Environment (getArgs)

main :: IO ()
main =
    do
        args <- getArgs
        if null args then runRepl else runFile args
