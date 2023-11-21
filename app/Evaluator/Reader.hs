module Evaluator.Reader where

import Control.Monad.Except
import Internal
import Parser.ParseExpression
import System.IO
import Text.ParserCombinators.Parsec as P

readExpr :: String -> ThrowsError Primitive
readExpr = readOrThrow parseExpression

readExprList :: String -> ThrowsError [Primitive]
readExprList = readOrThrow (P.endBy parseExpression P.spaces)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

readOrThrow :: P.Parser a -> String -> ThrowsError a
readOrThrow parser input = case P.parse parser "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val
