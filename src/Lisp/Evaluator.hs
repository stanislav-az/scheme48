{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

module Lisp.Evaluator where

import Control.Monad.Except
  ( MonadError(catchError, throwError)
  , MonadIO(liftIO)
  , liftEither
  , when
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
import Data.IORef (readIORef, writeIORef, newIORef)
import Control.Monad (forM)
import Data.List (foldl1')

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = pure val
eval env val@(Number _) = pure val
eval env val@(Float _) = pure val
eval env val@(Bool _) = pure val
eval env (Atom atom) = getVar env atom
eval env (List [Atom "quote", val]) = pure val
eval env (List [Atom "if", pred, conseq, alt]) = do
  result <- actualValue env pred
  case result of
    Bool False -> eval env alt
    _ -> eval env conseq
eval env (List (Atom "cond":clauses)) = liftEither (condToIf clauses) >>= eval env
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "load", String filename]) =
  load filename >>= fmap last . mapM (eval env)
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var
eval env val@(List (Atom "define":List (Atom var:params):body)) = do
  when (null body) $ throwError $
    SyntaxError "Lambda expression does not have body" val
  defineVar env var $ makeNormalFunc env params body
eval env val@(List (Atom "define":DottedList (Atom var:params) varargs:body)) = do
  when (null body) $ throwError $
    SyntaxError "Lambda expression does not have body" val
  defineVar env var $ makeVarArgs varargs env params body
eval env val@(List (Atom "lambda":List params:body)) = do
  when (null body) $ throwError $
    SyntaxError "Lambda expression does not have body" val
  pure $ makeNormalFunc env params body
eval env val@(List (Atom "lambda":DottedList params varargs:body)) = do
  when (null body) $ throwError $
    SyntaxError "Lambda expression does not have body" val
  pure $ makeVarArgs varargs env params body
eval env val@(List (Atom "lambda":varargs@(Atom _):body)) = do
  when (null body) $ throwError $
    SyntaxError "Lambda expression does not have body" val
  pure $ makeVarArgs varargs env [] body
eval env (List (function:args)) = do
  func <- actualValue env function
  apply env func args
eval env badForm =
  throwError $ BadSpecialForm "Unrecognized special form" badForm

actualValue :: Env -> LispVal -> IOThrowsError LispVal
actualValue env exp = forceIt =<< eval env exp

forceIt :: LispVal -> IOThrowsError LispVal
forceIt (Thunk tref) = do
  thunk <- liftIO $ readIORef tref
  case thunk of
    Right value -> pure value
    Left (env, unevaluated) -> do
      evaluated <- actualValue env unevaluated
      liftIO $ writeIORef tref $ Right evaluated
      pure evaluated
forceIt val = pure val

condToIf :: [LispVal] -> ThrowsError LispVal
condToIf [] = pure $ Bool False
condToIf (c:cs) =
  case c of
    List [Atom "else", expr] -> pure expr
    List [pred, conseq] -> do
      alt <- condToIf cs
      pure (List [Atom "if", pred, conseq, alt])
    badClause -> throwError $ SyntaxError "cond" badClause

apply :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
apply env (PrimitiveFunc func) args = liftEither . func =<< mapM (actualValue env) args
apply env (IOFunc func) args = func =<< mapM (actualValue env) args
apply env (Func Function {..}) exprs = do
  args <- forM exprs $ delayIt env
  if num params /= num args && isNothing vararg
    then throwError $ NumArgs (num params) args
    else liftIO (bindVars closure $ zip params args) >>= bindVarArgs args vararg >>=
         evalBody
  where
    remainingArgs = drop (length params)
    num = toInteger . length
    evalBody env = last <$> mapM (eval env) body
    bindVarArgs args arg env =
      case arg of
        Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs args)]
        Nothing -> return env
apply env badForm args = throwError $ NotFunction "Value is not a function" badForm

delayIt :: (MonadIO m) => Env -> LispVal -> m LispVal
delayIt env val = do
  tref <- liftIO $ newIORef $ Left (env, val)
  pure $ Thunk tref

primitiveBindings :: IO Env
primitiveBindings =
  nullEnv >>=
  flip
    bindVars
    (map (makeFunc IOFunc) ioPrimitives ++
     map (makeFunc PrimitiveFunc) primitives)
  where
    makeFunc constructor (var, func) = (var, constructor func)

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives =
  [ ("open-input-file", makePort ReadMode)
  , ("open-output-file", makePort WriteMode)
  , ("close-input-port", closePort)
  , ("close-output-port", closePort)
  , ("read", readProc)
  , ("write", writeProc)
  , ("read-contents", readContents)
  , ("read-all", readAll)
  ]

-- applyProc :: [LispVal] -> IOThrowsError LispVal
-- applyProc [func, List args] = apply func args
-- applyProc (func:args) = apply func args
-- applyProc [] = throwError $ NumArgs 1 []

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = fmap Port $ liftIO $ openFile filename mode
makePort mode [t] = throwError $ TypeMismatch "string" t
makePort mode args = throwError $ NumArgs 1 args

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = Bool True <$ liftIO (hClose port)
closePort [t] = throwError $ TypeMismatch "port" t
closePort args = throwError $ NumArgs 1 args

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftEither . readExpr
readProc [t] = throwError $ TypeMismatch "port" t
readProc args = throwError $ NumArgs 1 args

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = Bool True <$ liftIO (hPrint port obj)
writeProc [obj, t] = throwError $ TypeMismatch "port" t
writeProc args = throwError $ NumArgs 2 args

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = fmap String $ liftIO $ readFile filename
readContents [t] = throwError $ TypeMismatch "string" t
readContents args = throwError $ NumArgs 1 args

load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= liftEither . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = List <$> load filename
readAll [t] = throwError $ TypeMismatch "string" t
readAll args = throwError $ NumArgs 1 args

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", numericBinop (+))
  , ("-", numericBinop (-))
  , ("*", numericBinop (*))
  , ("/", integralBinop div)
  , ("/f", divFractionalBinop)
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
integralBinop op args = do
  ns <- mapM unpackInteger args
  if elem 0 $ drop 1 ns
    then throwError DividingByZero
    else pure . Number . foldl1' op $ ns

divFractionalBinop ::
     [LispVal] -> ThrowsError LispVal
divFractionalBinop [] = throwError $ NumArgs 2 []
divFractionalBinop singleVal@[_] = throwError $ NumArgs 2 singleVal
divFractionalBinop args = do
  ns <- mapM unpackDouble args
  if elem 0 $ drop 1 ns
    then throwError DividingByZero
    else pure . Float . foldl1' (/) $ ns

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
