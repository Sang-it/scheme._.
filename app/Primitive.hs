{-# LANGUAGE InstanceSigs #-}

module Primitive where

import           Control.Monad.Except (MonadError (catchError))
import           Internal             (Primitive (..),
                                       PrimitiveError (BadSpecialForm, NotFunction, NumArgs, Parser, TypeMismatch, UnboundVar),
                                       ThrowsError)

instance Show Primitive where
  show :: Primitive -> String
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
  show (IOFunc _) = "<IO primitive>"
  show (Port _) = "<IO port>"

instance Show PrimitiveError where
  show :: PrimitiveError -> String
  show (UnboundVar message varname) = message ++ ": " ++ varname
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NotFunction message func) = message ++ ": " ++ show func
  show (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwords (map show found)
  show (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
  show (Parser parseErr) = "Parse error at " ++ show parseErr

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
