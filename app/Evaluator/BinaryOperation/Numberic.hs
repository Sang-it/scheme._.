module Evaluator.BinaryOperation.Numberic (numericBinOp, unpackNum) where

import Control.Monad.Except
import Data.Functor
import Primitive.Primitive
import Primitive.PrimitiveError

numericBinOp :: (Integer -> Integer -> Integer) -> [Primitive] -> ThrowsError Primitive
numericBinOp op [] = throwError $ NumArgs 2 []
numericBinOp op val@[_] = throwError $ NumArgs 2 val
numericBinOp op params = mapM unpackNum params <&> (Number . foldl1 op)

unpackNum :: Primitive -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
  let parsed = reads n
   in if null parsed
        then throwError $ TypeMismatch "number" $ String n
        else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notANumber = throwError $ TypeMismatch "number" notANumber
