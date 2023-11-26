module Evaluator.BinaryOperation where

import Control.Monad.Except (MonadError (throwError))
import Data.Functor ((<&>))
import Evaluator.Unpacker (unpackBoolean, unpackNum, unpackString)
import Internal (
    Primitive (Bool, Number),
    PrimitiveError (NumArgs),
    ThrowsError,
 )

numericBinOp :: (Integer -> Integer -> Integer) -> [Primitive] -> ThrowsError Primitive
numericBinOp op [] = throwError $ NumArgs 2 []
numericBinOp op val@[_] = throwError $ NumArgs 2 val
numericBinOp op params = mapM unpackNum params <&> (Number . foldl1 op)

boolBinOp :: (Primitive -> ThrowsError a) -> (a -> a -> Bool) -> [Primitive] -> ThrowsError Primitive
boolBinOp unpacker op args =
    if length args /= 2
        then throwError $ NumArgs 2 args
        else do
            left <- unpacker $ head args
            right <- unpacker $ args !! 1
            return $ Bool $ left `op` right

numOrdBinOp :: (Integer -> Integer -> Bool) -> [Primitive] -> ThrowsError Primitive
numOrdBinOp = boolBinOp unpackNum

strOrdBinOp :: (String -> String -> Bool) -> [Primitive] -> ThrowsError Primitive
strOrdBinOp = boolBinOp unpackString

logicalBinOp :: (Bool -> Bool -> Bool) -> [Primitive] -> ThrowsError Primitive
logicalBinOp = boolBinOp unpackBoolean
