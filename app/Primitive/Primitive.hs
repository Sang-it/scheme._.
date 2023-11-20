module Primitive.Primitive where

import Control.Monad.Except
import qualified Text.ParserCombinators.Parsec as P

data Primitive
  = Atom String
  | List [Primitive]
  | DottedList [Primitive] Primitive
  | Number Integer
  | String String
  | Bool Bool

instance Show Primitive where
  show (Atom name) = name
  show (Number x) = show x
  show (String x) = "\"" ++ x ++ "\""
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (List xs) = "(" ++ unwords (map show xs) ++ ")"
  show (DottedList xs x) = "(" ++ unwords (map show xs) ++ " . " ++ show x ++ ")"

