module Evaluator.Repl where

import           Control.Monad.Except  (runExceptT)
import           Data.Functor          ((<&>))
import           Evaluator.Environment (bindVars, primitiveBindings,
                                        runIOThrows)
import           Evaluator.Eval        (eval, evalAndPrint, ioPrimitives,
                                        primitives)
import           Evaluator.Reader      (readPrompt)
import           Internal              (Env, Primitive (Atom, List, String))
import           System.IO             (hPutStrLn, stderr)

until_ :: (Monad m) => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

loadStandardLibrary :: [String] -> IO Env
loadStandardLibrary args =
  do
   env <- primitiveBindings primitives ioPrimitives >>= flip bindVars [("args", List $ map String $ drop 1 args)]
   runExceptT (eval env (List [Atom "load", String "lib/standard.scm"]))
   return env

runFile :: [String] -> IO ()
runFile args = do
  env <- loadStandardLibrary args
  runIOThrows (show <$> eval env (List [Atom "load", String (head args)])) >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = do
  env <- loadStandardLibrary []
  until_ (== "quit") (readPrompt "> ") (evalAndPrint env)
