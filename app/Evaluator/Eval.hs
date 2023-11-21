module Evaluator.Eval (eval) where

import Control.Monad.Except
import Evaluator.BinaryOperation.Numberic
import Evaluator.BinaryOperation.Ord
import Evaluator.Environment
import Evaluator.ListOperation
import Primitive.Primitive
import Primitive.PrimitiveError

primitives :: [(String, [Primitive] -> ThrowsError Primitive)]
primitives =
  [ ("+", numericBinOp (+)),
    ("-", numericBinOp (-)),
    ("*", numericBinOp (*)),
    ("/", numericBinOp div),
    ("mod", numericBinOp mod),
    ("quotient", numericBinOp quot),
    ("remainder", numericBinOp rem),
    ("=", numOrdBinOp (==)),
    ("<", numOrdBinOp (<)),
    (">", numOrdBinOp (>)),
    ("/=", numOrdBinOp (/=)),
    (">=", numOrdBinOp (>=)),
    ("<=", numOrdBinOp (<=)),
    ("&&", boolOrdBinOp (&&)),
    ("||", boolOrdBinOp (||)),
    ("string=?", strOrdBinOp (==)),
    ("string<?", strOrdBinOp (<)),
    ("string>?", strOrdBinOp (>)),
    ("string<=?", strOrdBinOp (<=)),
    ("string>=?", strOrdBinOp (>=)),
    ("car", car),
    ("cdr", cdr),
    ("cons", cons),
    ("eq?", eqv),
    ("eqv?", eqv),
    ("equal?", equal)
  ]

apply :: String -> [Primitive] -> ThrowsError Primitive
apply func args =
  maybe
    (throwError $ NotFunction "Unrecognized primitive function args" func)
    ($ args)
    $ lookup func primitives

eval :: Env -> Primitive -> IOThrowsError Primitive
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = do
  result <- eval env pred
  case result of
    Bool False -> eval env alt
    _ -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom func : args)) = mapM (eval env) args >>= liftThrows . apply func
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form: " badForm
