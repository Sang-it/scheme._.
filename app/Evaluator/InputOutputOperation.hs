module Evaluator.InputOutputOperation where

import           Control.Monad.Except  (MonadIO (liftIO))
import           Evaluator.Environment (liftThrows)
import           Evaluator.Reader      (readExpr, readExprList)
import           Internal              (IOThrowsError,
                                        Primitive (Bool, List, Port, String))
import           System.IO             (IOMode, hClose, hGetLine, hPrint,
                                        openFile, stdin, stdout)

makePort :: IOMode -> [Primitive] -> IOThrowsError Primitive
makePort mode [String filename] = fmap Port $ liftIO $ openFile filename mode

closePort :: [Primitive] -> IOThrowsError Primitive
closePort [Port port] = liftIO $ hClose port >> return (Bool True)
closePort _           = return $ Bool False

readProc  []          = readProc [Port stdin]
readProc  [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr

writeProc :: [Primitive] -> IOThrowsError Primitive
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> return (Bool True)

readContents :: [Primitive] -> IOThrowsError Primitive
readContents [String filename] = String <$> liftIO (readFile filename)

load :: String -> IOThrowsError [Primitive]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList

readAll :: [Primitive] -> IOThrowsError Primitive
readAll [String filename] = List <$> load filename
