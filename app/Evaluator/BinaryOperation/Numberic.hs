module Evaluator.BinaryOperation.Numberic (numericBinOp) where

import Control.Monad.Except
import Data.Functor
import Evaluator.Unpacker (unpackNum)
import Internal

numericBinOp :: (Integer -> Integer -> Integer) -> [Primitive] -> ThrowsError Primitive
numericBinOp op [] = throwError $ NumArgs 2 []
numericBinOp op val@[_] = throwError $ NumArgs 2 val
numericBinOp op params = mapM unpackNum params <&> (Number . foldl1 op)
