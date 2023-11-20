module Main where

import Evaluator.Eval
import Evaluator.Repl
import Primitive.PrimitiveError
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> evalAndPrint $ head args
    _ -> putStrLn "Program takes only 0 or 1 argument"
