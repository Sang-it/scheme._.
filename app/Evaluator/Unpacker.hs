{-# LANGUAGE ExistentialQuantification #-}

module Evaluator.Unpacker where

import Control.Monad.Except
import Internal

data Unpacker = forall a. (Eq a) => AnyUnpacker (Primitive -> ThrowsError a)

unpackEquals :: Primitive -> Primitive -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do
    unpacked1 <- unpacker arg1
    unpacked2 <- unpacker arg2
    return $ unpacked1 == unpacked2
    `catchError` const (return False)

unpackBoolean :: Primitive -> ThrowsError Bool
unpackBoolean (Bool b) = return b
unpackBoolean notABoolean = throwError $ TypeMismatch "boolean" notABoolean

unpackString :: Primitive -> ThrowsError String
unpackString (String s) = return s
unpackString (Number s) = return $ show s
unpackString (Bool s) = return $ show s
unpackString notAString = throwError $ TypeMismatch "string" notAString

unpackNum :: Primitive -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
  let parsed = reads n
   in if null parsed
        then throwError $ TypeMismatch "number" $ String n
        else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notANumber = throwError $ TypeMismatch "number" notANumber
