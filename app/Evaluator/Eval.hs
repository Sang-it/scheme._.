module Evaluator.Eval (eval) where

import Control.Monad.Except
import Evaluator.BinaryOperation.Numberic
import Evaluator.BinaryOperation.Ord
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

eval :: Primitive -> ThrowsError Primitive
eval val@(String _) = return val
-- eval val@(Atom _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = do
  result <- eval pred
  case result of
    Bool False -> eval alt
    _ -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form: " badForm
