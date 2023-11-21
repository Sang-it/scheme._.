module Primitive.Primitive where

import Control.Monad.Except
import Internal
import qualified Text.ParserCombinators.Parsec as P

instance Show Primitive where
  show (Atom name) = name
  show (Number x) = show x
  show (String x) = "\"" ++ x ++ "\""
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (List xs) = "(" ++ unwords (map show xs) ++ ")"
  show (DottedList xs x) = "(" ++ unwords (map show xs) ++ " . " ++ show x ++ ")"
  show (PrimitiveFunc _) = "<primitive>"
  show (Func {params = args, vararg = varargs, body = _, closure = _}) =
    "(lambda (" ++ unwords (map show args) ++ (case varargs of Nothing -> ""; Just arg -> " . " ++ arg) ++ ") ...)"
