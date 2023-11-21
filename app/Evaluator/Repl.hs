module Evaluator.Repl where

import           Evaluator.Environment (bindVars, runIOThrows)
import           Evaluator.Eval        (eval, evalAndPrint, primitiveBindings)
import           Evaluator.Reader      (readPrompt)
import           Internal              (Primitive (Atom, List, String))
import           System.IO             (hPutStrLn, stderr)

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
