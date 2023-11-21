module Evaluator.BinaryOperation.Numberic (numericBinOp) where

import           Control.Monad.Except (MonadError (throwError))
import           Data.Functor         ((<&>))
import           Evaluator.Unpacker   (unpackNum)
import           Internal             (Primitive (Number),
                                       PrimitiveError (NumArgs), ThrowsError)

numericBinOp :: (Integer -> Integer -> Integer) -> [Primitive] -> ThrowsError Primitive
numericBinOp op []      = throwError $ NumArgs 2 []
numericBinOp op val@[_] = throwError $ NumArgs 2 val
numericBinOp op params  = mapM unpackNum params <&> (Number . foldl1 op)
