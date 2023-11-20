module Main where

import Control.Applicative
import Control.Monad.Except
import Evaluator.Eval
import Parser.ParseExpression
import Primitive.Primitive
import Primitive.PrimitiveError
import System.Environment
import qualified Text.ParserCombinators.Parsec as P

readExpr :: String -> ThrowsError Primitive
readExpr input = case P.parse parseExpression "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

main :: IO ()
main = do
  args <- getArgs
  let evaled = fmap show $ readExpr (head args) >>= eval
  putStrLn $ extractValue $ trapError evaled
