module Evaluator.Eval where

import Control.Monad.Except (
    ExceptT,
    MonadError (throwError),
    MonadIO (liftIO),
 )
import Data.Maybe (isNothing)
import Evaluator.BinaryOperation (
    logicalBinOp,
    numOrdBinOp,
    numericBinOp,
    strOrdBinOp,
 )
import Evaluator.Environment (
    bindVars,
    defineVar,
    getVar,
    liftThrows,
    makeNormalFunc,
    makeVarargs,
    nullEnv,
    runIOThrows,
    setVar,
 )
import Evaluator.InputOutputOperation (
    closePort,
    load,
    makePort,
    readAll,
    readContents,
    readProc,
    writeProc,
 )
import Evaluator.ListOperation (car, cdr, cons, equal, eqv)
import Evaluator.Reader (readExpr)
import Internal (
    Env,
    IOThrowsError,
    Primitive (Atom, Bool, DottedList, Func, IOFunc, List, Number, PrimitiveFunc, String),
    PrimitiveError (BadSpecialForm, NumArgs),
    ThrowsError,
 )
import System.IO (IOMode (ReadMode, WriteMode))

primitives :: [(String, [Primitive] -> ThrowsError Primitive)]
primitives =
    [ -- Numeric operations
      ("+", numericBinOp (+))
    , ("-", numericBinOp (-))
    , ("*", numericBinOp (*))
    , ("/", numericBinOp div)
    , ("mod", numericBinOp mod)
    , ("quotient", numericBinOp quot)
    , ("remainder", numericBinOp rem)
    , -- Numeric order operations
      ("=", numOrdBinOp (==))
    , ("<", numOrdBinOp (<))
    , (">", numOrdBinOp (>))
    , ("/=", numOrdBinOp (/=))
    , (">=", numOrdBinOp (>=))
    , ("<=", numOrdBinOp (<=))
    , -- String order operations
      ("string=?", strOrdBinOp (==))
    , ("string<?", strOrdBinOp (<))
    , ("string>?", strOrdBinOp (>))
    , ("string<=?", strOrdBinOp (<=))
    , ("string>=?", strOrdBinOp (>=))
    , -- Logical operations
      ("&&", logicalBinOp (&&))
    , ("||", logicalBinOp (||))
    , -- List operations
      ("car", car)
    , ("cdr", cdr)
    , ("cons", cons)
    , ("eq?", eqv)
    , ("eqv?", eqv)
    , ("equal?", equal)
    ]

ioPrimitives :: [(String, [Primitive] -> IOThrowsError Primitive)]
ioPrimitives =
    [ ("apply", applyProc)
    , ("open-input-file", makePort ReadMode)
    , ("open-output-file", makePort WriteMode)
    , ("close-input-port", closePort)
    , ("close-output-port", closePort)
    , ("read", readProc)
    , ("write", writeProc)
    , ("read-contents", readContents)
    , ("read-all", readAll)
    ]

apply :: Primitive -> [Primitive] -> IOThrowsError Primitive
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
    if num params /= num args && isNothing varargs
        then throwError $ NumArgs (num params) args
        else liftIO (bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
  where
    remainingArgs = drop (length params) args
    num = toInteger . length
    evalBody env = last <$> mapM (eval env) body
    bindVarArgs arg env =
        case arg of
            Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
            Nothing -> return env
apply (IOFunc func) args = func args

applyProc :: [Primitive] -> IOThrowsError Primitive
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

eval :: Env -> Primitive -> IOThrowsError Primitive
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "load", String filename]) = load filename >>= fmap last . mapM (eval env)
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = do
    result <- eval env pred
    case result of
        Bool False -> eval env alt
        _ -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) = makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) = makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) = makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) = makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) = makeVarargs varargs env [] body
eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form: " badForm

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn
