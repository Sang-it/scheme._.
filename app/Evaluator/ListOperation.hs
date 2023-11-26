module Evaluator.ListOperation where

import Control.Monad.Except (MonadError (throwError))
import Evaluator.Unpacker (
    Unpacker (AnyUnpacker),
    unpackBoolean,
    unpackEquals,
    unpackNum,
    unpackString,
 )
import Internal (
    Primitive (Atom, Bool, DottedList, List, Number, String),
    PrimitiveError (NumArgs, TypeMismatch),
    ThrowsError,
 )

car :: [Primitive] -> ThrowsError Primitive
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [Primitive] -> ThrowsError Primitive
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [notList] = throwError $ TypeMismatch "pair" notList
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [Primitive] -> ThrowsError Primitive
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [Primitive] -> ThrowsError Primitive
eqv [Bool arg1, Bool arg2] = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2] = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2] = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2] = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2] = return $ Bool $ length arg1 == length arg2 && all eqvPair (zip arg1 arg2)
  where
    eqvPair (x1, x2) = case eqv [x1, x2] of
        Left err -> False
        Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

equal :: [Primitive] -> ThrowsError Primitive
equal [arg1, arg2] = do
    primitiveEquals <-
        or
            <$> mapM
                (unpackEquals arg1 arg2)
                [AnyUnpacker unpackNum, AnyUnpacker unpackString, AnyUnpacker unpackBoolean]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool $ primitiveEquals || let (Bool x) = eqvEquals in x
equal badArgList = throwError $ NumArgs 2 badArgList
