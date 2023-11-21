module Evaluator.Repl where

import Control.Monad.Except
import Evaluator.Environment
import Evaluator.Eval
import Evaluator.Reader
import Internal
import Parser.ParseExpression
import System.Environment
import System.IO
import qualified Text.ParserCombinators.Parsec as P

until_ :: (Monad m) => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

runOne :: [String] -> IO ()
runOne args = do
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  runIOThrows (show <$> eval env (List [Atom "load", String (head args)])) >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lang > ") . evalAndPrint
