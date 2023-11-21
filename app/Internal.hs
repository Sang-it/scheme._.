-- A Internal Module used to create globally used types and functions to avoid cyclic dependencies.
-- I maybe will switch to .hs-boot files later, but this seems to be the preferred way.
-- TODO: Maybe create a .hs-boot file to resolve the cyclic dependency in the future.
module Internal where

import           Control.Monad.Except          (ExceptT)
import           Data.IORef                    (IORef)
import           System.IO                     (Handle)
import           Text.ParserCombinators.Parsec (ParseError)

data Primitive
  = Atom String
  | List [Primitive]
  | DottedList [Primitive] Primitive
  | Number Integer
  | String String
  | Bool Bool
  | PrimitiveFunc ([Primitive] -> ThrowsError Primitive)
  | Func
      { params  :: [String],
        vararg  :: Maybe String,
        body    :: [Primitive],
        closure :: Env
      }
  | IOFunc ([Primitive] -> IOThrowsError Primitive)
  | Port Handle

data PrimitiveError
  = NumArgs Integer [Primitive]
  | TypeMismatch String Primitive
  | Parser ParseError
  | BadSpecialForm String Primitive
  | NotFunction String String
  | UnboundVar String String
  | Default String

type Env = IORef [(String, IORef Primitive)]

type ThrowsError = Either PrimitiveError

type IOThrowsError = ExceptT PrimitiveError IO
