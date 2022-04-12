{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

module Lisp.Evaluator where

import Control.Monad.Except
  ( MonadError(catchError, throwError)
  , MonadIO(liftIO)
  , liftEither
  , when, runExceptT
  )
import Data.Functor ((<&>))
import Data.Maybe (isNothing)
import Lisp.Core
import Lisp.Parse (readExpr, readExprList)
import System.IO
  ( IOMode(ReadMode, WriteMode)
  , hClose
  , hGetLine
  , hPrint
  , openFile
  , stdin
  , stdout
  )
import Control.Monad.State
import Data.Function ((&))
import qualified Data.Map as M

eval :: LispVal -> StateThrowsError LispVal
eval val@(String _) = pure val
eval val@(Number _) = pure val
eval val@(Float _) = pure val
eval val@(Bool _) = pure val
eval (Atom atom) = get >>= getVar atom
eval (List [Atom "quote", val]) = pure val
eval (List [Atom "if", pred, conseq, alt]) = do
  result <- eval pred
  case result of
    Bool False -> eval alt
    _ -> eval conseq
eval (List (Atom "cond":clauses)) = condToIf clauses
eval (List [Atom "set!", Atom var, form]) = do
  val <- eval form
  env <- get
  setVar env var val
eval (List [Atom "load", String filename]) =
  load filename >>= fmap last . mapM eval
eval (List [Atom "define", Atom var, form]) = do
  val <- eval form
  env <- get
  defineVar env var val
eval val@(List (Atom "define":List (Atom var:params):body)) = do
  when (null body) $ throwError $
    SyntaxError "Lambda expression does not have body" val
  env <- get
  -- Curious case: func is defined in terms of fnclosure and the other way around
  let func = makeNormalFunc fnclosure params body
      fnclosure = M.insert var func env
  defineVar env var func
eval val@(List (Atom "define":DottedList (Atom var:params) varargs:body)) = do
  when (null body) $ throwError $
    SyntaxError "Lambda expression does not have body" val
  env <- get
  let func = makeVarArgs varargs fnclosure params body
      fnclosure = M.insert var func env
  defineVar env var func
eval val@(List (Atom "lambda":List params:body)) = do
  when (null body) $ throwError $
    SyntaxError "Lambda expression does not have body" val
  env <- get
  pure $ makeNormalFunc env params body
eval val@(List (Atom "lambda":DottedList params varargs:body)) = do
  when (null body) $ throwError $
    SyntaxError "Lambda expression does not have body" val
  env <- get
  pure $ makeVarArgs varargs env params body
eval val@(List (Atom "lambda":varargs@(Atom _):body)) = do
  when (null body) $ throwError $
    SyntaxError "Lambda expression does not have body" val
  env <- get
  pure $ makeVarArgs varargs env [] body
eval (List (function:args)) = do
  func <- eval function
  argVals <- mapM (eval) args
  apply func argVals
eval badForm =
  throwError $ BadSpecialForm "Unrecognized special form" badForm

condToIf :: [LispVal] -> StateThrowsError LispVal
condToIf [] = pure $ Bool False
condToIf (c:cs) =
  case c of
    List [Atom "else", expr] -> eval expr
    List [pred, conseq] -> do
      alt <- condToIf cs
      eval (List [Atom "if", pred, conseq, alt])
    badClause -> throwError $ SyntaxError "cond" badClause

apply :: LispVal -> [LispVal] -> StateThrowsError LispVal
apply (PrimitiveFunc func) args = liftEither $ func args
apply (IOFunc func) args = func args
apply (Func Function {..}) args =
  if num params /= num args && isNothing vararg
    then throwError $ NumArgs (num params) args
    else do
      res <- liftIO $ flip evalStateT newEnv $ runExceptT evalBody
      either throwError pure res
  where
    newEnv = bindVars closure (zip params args) & case vararg of
      Just argName -> flip bindVars [(argName, List remainingArgs)]
      Nothing -> id
    remainingArgs = drop (length params) args
    num = toInteger . length
    evalBody = last <$> mapM eval body

apply badForm args = throwError $ NotFunction "Value is not a function" badForm

primitiveBindings :: Env
primitiveBindings =
    bindVars nullEnv
    (map (makeFunc IOFunc) ioPrimitives ++
     map (makeFunc PrimitiveFunc) primitives)
  where
    makeFunc constructor (var, func) = (var, constructor func)

ioPrimitives :: [(String, [LispVal] -> StateThrowsError LispVal)]
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

applyProc :: [LispVal] -> StateThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func:args) = apply func args
applyProc [] = throwError $ NumArgs 1 []

makePort :: IOMode -> [LispVal] -> StateThrowsError LispVal
makePort mode [String filename] = fmap Port $ liftIO $ openFile filename mode
makePort mode [t] = throwError $ TypeMismatch "string" t
makePort mode args = throwError $ NumArgs 1 args

closePort :: [LispVal] -> StateThrowsError LispVal
closePort [Port port] = Bool True <$ liftIO (hClose port)
closePort [t] = throwError $ TypeMismatch "port" t
closePort args = throwError $ NumArgs 1 args

readProc :: [LispVal] -> StateThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftEither . readExpr
readProc [t] = throwError $ TypeMismatch "port" t
readProc args = throwError $ NumArgs 1 args

writeProc :: [LispVal] -> StateThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = Bool True <$ liftIO (hPrint port obj)
writeProc [obj, t] = throwError $ TypeMismatch "port" t
writeProc args = throwError $ NumArgs 2 args

readContents :: [LispVal] -> StateThrowsError LispVal
readContents [String filename] = fmap String $ liftIO $ readFile filename
readContents [t] = throwError $ TypeMismatch "string" t
readContents args = throwError $ NumArgs 1 args

load :: String -> StateThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= liftEither . readExprList

readAll :: [LispVal] -> StateThrowsError LispVal
readAll [String filename] = List <$> load filename
readAll [t] = throwError $ TypeMismatch "string" t
readAll args = throwError $ NumArgs 1 args

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", numericBinop (+))
  , ("-", numericBinop (-))
  , ("*", numericBinop (*))
  , ("/", integralBinop div)
  , ("/.", fractionalBinop (/))
  , ("mod", integralBinop mod)
  , ("quotient", integralBinop quot)
  , ("remainder", integralBinop rem)
  , ("=", numBoolBinop (==))
  , ("<", numBoolBinop (<))
  , (">", numBoolBinop (>))
  , ("/=", numBoolBinop (/=))
  , (">=", numBoolBinop (>=))
  , ("<=", numBoolBinop (<=))
  , ("&&", boolBoolBinop (&&))
  , ("||", boolBoolBinop (||))
  , ("string=?", strBoolBinop (==))
  , ("string<?", strBoolBinop (<))
  , ("string>?", strBoolBinop (>))
  , ("string<=?", strBoolBinop (<=))
  , ("string>=?", strBoolBinop (>=))
  , ("car", car)
  , ("cdr", cdr)
  , ("cons", cons)
  , ("eq?", eqv)
  , ("eqv?", eqv)
  , ("equal?", equal)
  ]

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

boolBinop ::
     (LispVal -> ThrowsError a)
  -> (a -> a -> Bool)
  -> [LispVal]
  -> ThrowsError LispVal
boolBinop unpacker op args =
  if length args /= 2
    then throwError $ NumArgs 2 args
    else do
      left <- unpacker $ head args
      right <- unpacker $ args !! 1
      return $ Bool $ left `op` right

numBoolBinop ::
     (forall a. Ord a =>
                  a -> a -> Bool)
  -> [LispVal]
  -> ThrowsError LispVal
numBoolBinop op args =
  if length args /= 2
    then throwError $ NumArgs 2 args
    else case head args of
           Number n -> Bool . op n <$> unpackInteger (args !! 1)
           Float n -> Bool . op n <$> unpackDouble (args !! 1)
           notNum -> throwError $ TypeMismatch "number or float" notNum

numericBinop ::
     (forall a. Num a =>
                  a -> a -> a)
  -> [LispVal]
  -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op (a:args) =
  case a of
    Number i -> mapM unpackInteger args <&> Number . foldl op i
    Float t -> mapM unpackDouble args <&> Float . foldl op t
    notNum -> throwError $ TypeMismatch "number or float" notNum

integralBinop ::
     (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
integralBinop op [] = throwError $ NumArgs 2 []
integralBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
integralBinop op args = mapM unpackInteger args <&> Number . foldl1 op

fractionalBinop ::
     (Double -> Double -> Double) -> [LispVal] -> ThrowsError LispVal
fractionalBinop op [] = throwError $ NumArgs 2 []
fractionalBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
fractionalBinop op args = mapM unpackDouble args <&> Float . foldl1 op

unpackInteger :: LispVal -> ThrowsError Integer
unpackInteger (Number i) = pure i
unpackInteger notNum = throwError $ TypeMismatch "number" notNum

unpackIntegerWeak :: LispVal -> ThrowsError Integer
unpackIntegerWeak (Number n) = return n
unpackIntegerWeak (String n) =
  let parsed = reads n
   in if null parsed
        then throwError $ TypeMismatch "number" $ String n
        else return $ fst $ parsed !! 0
unpackIntegerWeak (List [n]) = unpackIntegerWeak n
unpackIntegerWeak notNum = throwError $ TypeMismatch "number" notNum

unpackDouble :: LispVal -> ThrowsError Double
unpackDouble (Float d) = pure d
unpackDouble notFloat = throwError $ TypeMismatch "float" notFloat

unpackDoubleWeak :: LispVal -> ThrowsError Double
unpackDoubleWeak (Float n) = return n
unpackDoubleWeak (String n) =
  let parsed = reads n
   in if null parsed
        then throwError $ TypeMismatch "float" $ String n
        else return $ fst $ parsed !! 0
unpackDoubleWeak (List [n]) = unpackDoubleWeak n
unpackDoubleWeak notNum = throwError $ TypeMismatch "float" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackStrWeak :: LispVal -> ThrowsError String
unpackStrWeak (String s) = return s
unpackStrWeak (Number s) = return $ show s
unpackStrWeak (Bool s) = return $ show s
unpackStrWeak notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2] = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2] = return $ Bool $ arg1 == arg2
eqv [Float arg1, Float arg2] = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2] = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2] = return $ Bool $ arg1 == arg2
eqv [xs@(DottedList _ _), ys@(DottedList _ _)] = eqvList eqv xs ys
eqv [xs@(List _), ys@(List _)] = eqvList eqv xs ys
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

eqvList ::
     ([LispVal] -> ThrowsError LispVal)
  -> LispVal
  -> LispVal
  -> ThrowsError LispVal
eqvList testEq (DottedList xs x) (DottedList ys y) =
  testEq [List $ xs ++ [x], List $ ys ++ [y]]
eqvList testEq (List arg1) (List arg2) =
  return $ Bool $ length arg1 == length arg2 && all eqvPair (zip arg1 arg2)
  where
    eqvPair (x1, x2) =
      case testEq [x1, x2] of
        Right (Bool val) -> val
        _ -> False
eqvList _ x y = throwError $ TypeMismatch "list" (List [x, y])

data Unpacker =
  forall a. Eq a =>
            AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do unpacked1 <- unpacker arg1
     unpacked2 <- unpacker arg2
     return $ unpacked1 == unpacked2
     `catchError` const (return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [xs@(DottedList _ _), ys@(DottedList _ _)] = eqvList equal xs ys
equal [xs@(List _), ys@(List _)] = eqvList equal xs ys
equal [arg1, arg2] = do
  primitiveEquals <-
    or <$>
    mapM
      (unpackEquals arg1 arg2)
      [ AnyUnpacker unpackIntegerWeak
      , AnyUnpacker unpackDoubleWeak
      , AnyUnpacker unpackStrWeak
      , AnyUnpacker unpackBool
      ]
  eqvEquals <- eqv [arg1, arg2]
  return $
    Bool
      (primitiveEquals ||
       let (Bool x) = eqvEquals
        in x)
equal badArgList = throwError $ NumArgs 2 badArgList
