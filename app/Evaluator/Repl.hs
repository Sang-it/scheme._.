module Evaluator.Repl where

import Control.Monad.Except
import Evaluator.Environment
import Evaluator.Eval
import Parser.ParseExpression
import Primitive.Primitive
import Primitive.PrimitiveError
import System.Environment
import System.IO
import qualified Text.ParserCombinators.Parsec as P

readExpr :: String -> ThrowsError Primitive
readExpr input = case P.parse parseExpression "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: (Monad m) => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

runOne :: String -> IO ()
runOne arg = nullEnv >>= flip evalAndPrint arg

runRepl :: IO ()
runRepl = nullEnv >>= until_ (== "quit") (readPrompt "Lang>>> ") . evalAndPrint
