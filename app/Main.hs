module Main where

import Evaluator.Eval
import Evaluator.Repl
import Primitive.PrimitiveError
import System.Environment

main :: IO ()
main =
  do
    args <- getArgs
    if null args then runRepl else runOne args
