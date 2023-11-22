module Evaluator.Reader where

import           Control.Monad.Except
import           Internal                      (Primitive,
                                                PrimitiveError (Parser),
                                                ThrowsError)
import           Parser                        (parseExpression)
import           System.IO                     (hFlush, stdout)
import           Text.ParserCombinators.Parsec as P (Parser, endBy, parse,
                                                     spaces)

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
  Left err  -> throwError $ Parser err
  Right val -> return val
