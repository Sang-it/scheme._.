module Evaluator.Environment where

import Control.Monad.Except (
    ExceptT,
    MonadError (throwError),
    MonadIO (liftIO),
    runExceptT,
 )
import Data.Functor ((<&>))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import Evaluator.ListOperation
import Internal (
    Env,
    IOThrowsError,
    Primitive (Func, IOFunc, List, PrimitiveFunc),
    PrimitiveError (NumArgs, UnboundVar),
    ThrowsError,
 )
import Primitive (extractValue, trapError)

-- Variable operations
nullEnv :: IO Env
nullEnv = newIORef []

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) <&> extractValue

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef <&> (isJust . lookup var)

getVar :: Env -> String -> IOThrowsError Primitive
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    maybe
        (throwError $ UnboundVar "Getting an unbound variable" var)
        (liftIO . readIORef)
        (lookup var env)

setVar :: Env -> String -> Primitive -> IOThrowsError Primitive
setVar envRef var value = do
    env <- liftIO $ readIORef envRef
    maybe
        (throwError $ UnboundVar "Setting an unbound variable" var)
        (liftIO . flip writeIORef value)
        (lookup var env)
    return value

defineVar :: Env -> String -> Primitive -> IOThrowsError Primitive
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
            valueRef <- newIORef value
            env <- readIORef envRef
            writeIORef envRef ((var, valueRef) : env)
            return value

bindVars :: Env -> [(String, Primitive)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv bindings env = fmap (++ env) (mapM addBinding bindings)
    addBinding (var, value) = do
        ref <- newIORef value
        return (var, ref)

primitiveBindings :: [(String, [Primitive] -> ThrowsError Primitive)] -> [(String, [Primitive] -> IOThrowsError Primitive)] -> IO Env
primitiveBindings primitives ioPrimitives =
    nullEnv
        >>= flip bindVars (map (makeFunc IOFunc) ioPrimitives ++ map (makeFunc PrimitiveFunc) primitives)
  where
    makeFunc constructor (var, func) = (var, constructor func)

-- Function operations
makeFunc :: (Monad m, Show a) => Maybe String -> Env -> [a] -> [Primitive] -> m Primitive
makeFunc varargs env params body = return $ Func (map show params) varargs body env

makeNormalFunc :: Env -> [Primitive] -> [Primitive] -> ExceptT PrimitiveError IO Primitive
makeNormalFunc = makeFunc Nothing

makeVarargs :: Primitive -> Env -> [Primitive] -> [Primitive] -> ExceptT PrimitiveError IO Primitive
makeVarargs = makeFunc . Just . show
