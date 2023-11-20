module Evaluator.BinaryOperation.Ord (numOrdBinOp, strOrdBinOp, boolOrdBinOp) where

import Control.Monad.Except
import Evaluator.BinaryOperation.Numberic (unpackNum)
import Primitive.Primitive
import Primitive.PrimitiveError

boolBinOp :: (Primitive -> ThrowsError a) -> (a -> a -> Bool) -> [Primitive] -> ThrowsError Primitive
boolBinOp unpacker op args =
  if length args /= 2
    then throwError $ NumArgs 2 args
    else do
      left <- unpacker $ head args
      right <- unpacker $ args !! 1
      return $ Bool $ left `op` right

unpackBoolean :: Primitive -> ThrowsError Bool
unpackBoolean (Bool b) = return b
unpackBoolean notABoolean = throwError $ TypeMismatch "boolean" notABoolean

unpackString :: Primitive -> ThrowsError String
unpackString (String s) = return s
unpackString (Number s) = return $ show s
unpackString (Bool s) = return $ show s
unpackString notAString = throwError $ TypeMismatch "string" notAString

numOrdBinOp :: (Integer -> Integer -> Bool) -> [Primitive] -> ThrowsError Primitive
numOrdBinOp = boolBinOp unpackNum

strOrdBinOp :: (String -> String -> Bool) -> [Primitive] -> ThrowsError Primitive
strOrdBinOp = boolBinOp unpackString

boolOrdBinOp :: (Bool -> Bool -> Bool) -> [Primitive] -> ThrowsError Primitive
boolOrdBinOp = boolBinOp unpackBoolean
