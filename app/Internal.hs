-- A Internal Module used to create globally used types and functions to avoid cyclic dependencies.
-- I maybe will switch to .hs-boot files later, but this seems to be the preferred way.
-- TODO: Maybe create a .hs-boot file to resolve the cyclic dependency in the future.
module Internal where

import Data.IORef
import qualified Text.ParserCombinators.Parsec as P

data Primitive
  = Atom String
  | List [Primitive]
  | DottedList [Primitive] Primitive
  | Number Integer
  | String String
  | Bool Bool
  | PrimitiveFunc ([Primitive] -> ThrowsError Primitive)
  | Func
      { params :: [String],
        vararg :: Maybe String,
        body :: [Primitive],
        closure :: Env
      }

data PrimitiveError
  = NumArgs Integer [Primitive]
  | TypeMismatch String Primitive
  | Parser P.ParseError
  | BadSpecialForm String Primitive
  | NotFunction String String
  | UnboundVar String String
  | Default String

type Env = IORef [(String, IORef Primitive)]

type ThrowsError = Either PrimitiveError
