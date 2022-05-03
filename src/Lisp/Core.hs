{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Lisp.Core where

import Control.Monad.Except
  ( ExceptT
  , MonadError(throwError)
  , MonadIO(liftIO)
  , runExceptT
  )
import Data.Functor ((<&>))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import System.IO (Handle)
import qualified Text.ParserCombinators.Parsec as P

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | Float Double
  | String String
  | Bool Bool
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | Func Function
  | IOFunc ([LispVal] -> IOThrowsError LispVal)
  | Port Handle

instance Show LispVal where
  show = showVal

data Function =
  Function
    { params :: [String]
    , vararg :: Maybe String
    , body :: [LispVal]
    , closure :: Env
    }

makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> LispVal
makeFunc vararg closure ps body =
  let params = map showVal ps
   in Func Function {..}

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: LispVal -> Env -> [LispVal] -> [LispVal] -> LispVal
makeVarArgs = makeFunc . Just . showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) =
  "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func Function {..}) =
  "(lambda (" ++
  unwords (map show params) ++
  (case vararg of
     Nothing -> ""
     Just arg -> " . " ++ arg) ++
  ") ...)"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser P.ParseError
  | BadSpecialForm String LispVal
  | NotFunction String LispVal
  | UnboundVar String String
  | SyntaxError String LispVal
  | DividingByZero
  | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) =
  "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) =
  "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (SyntaxError form ctx) =
  "Syntax error in special form \"" ++ form ++ "\" near " ++ show ctx
showError DividingByZero = "Divide by zero exception"
showError (Default e) = e

instance Show LispError where
  show = showError

type Env = IORef [(String, IORef LispVal)]

type ThrowsError = Either LispError

type IOThrowsError = ExceptT LispError IO

nullEnv :: IO Env
nullEnv = newIORef []

runIOThrows :: (Show a) => IOThrowsError a -> IO String
runIOThrows = fmap (either show show) . runExceptT

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef <&> (isJust . lookup var)

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Getting an unbound variable" var)
    (liftIO . readIORef)
    (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Setting an unbound variable" var)
    (liftIO . flip writeIORef value)
    (lookup var env)
  return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
           valueRef <- newIORef value
           env <- readIORef envRef
           writeIORef envRef ((var, valueRef) : env)
           return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv bindings env = fmap (++ env) (mapM addBinding bindings)
    addBinding (var, value) = do
      ref <- newIORef value
      return (var, ref)
