module Primitive.PrimitiveError where

import Control.Monad.Except
import Primitive.Primitive
import qualified Text.ParserCombinators.Parsec as P

data PrimitiveError
  = NumArgs Integer [Primitive]
  | TypeMismatch String Primitive
  | Parser P.ParseError
  | BadSpecialForm String Primitive
  | NotFunction String String
  | UnboundVar String String
  | Default String

instance Show PrimitiveError where
  show (UnboundVar message varname) = message ++ ": " ++ varname
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NotFunction message func) = message ++ ": " ++ show func
  show (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwords (map show found)
  show (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
  show (Parser parseErr) = "Parse error at " ++ show parseErr

type ThrowsError = Either PrimitiveError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
