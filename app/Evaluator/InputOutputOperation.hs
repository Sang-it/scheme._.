module Evaluator.InputOutputOperation where

import Control.Monad.Except
import Evaluator.Environment
import Evaluator.Reader
import Internal
import System.IO

makePort :: IOMode -> [Primitive] -> IOThrowsError Primitive
makePort mode [String filename] = fmap Port $ liftIO $ openFile filename mode

closePort :: [Primitive] -> IOThrowsError Primitive
closePort [Port port] = liftIO $ hClose port >> return (Bool True)
closePort _ = return $ Bool False

readProc :: [Primitive] -> IOThrowsError Primitive
readProc [] = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr

writeProc :: [Primitive] -> IOThrowsError Primitive
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> return (Bool True)

readContents :: [Primitive] -> IOThrowsError Primitive
readContents [String filename] = String <$> liftIO (readFile filename)

load :: String -> IOThrowsError [Primitive]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList

readAll :: [Primitive] -> IOThrowsError Primitive
readAll [String filename] = List <$> load filename
