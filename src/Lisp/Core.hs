{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Lisp.Core where

import Control.Monad.Except
  ( ExceptT
  , MonadError(throwError)
  , MonadIO(liftIO)
  , runExceptT
  )
import Data.Functor ((<&>))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isJust, isNothing)
import System.IO (Handle)
import qualified Text.ParserCombinators.Parsec as P
import qualified Data.Map as M
import Control.Monad.State
import qualified Control.Lens as Lens
import qualified Data.Set as Set

type ThrowsError = Either LispError

type StateThrowsError = ExceptT LispError (StateT Env IO)

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
  | IOFunc ([LispVal] -> StateThrowsError LispVal)
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

data Env = Env {
  dict :: M.Map String LispVal,
  defined :: Set.Set String
  }

Lens.makeClassy_ ''Env

nullEnv :: Env
nullEnv = Env M.empty Set.empty

runIOThrows :: (Show a) => Env -> StateThrowsError a -> IO (String, Env)
runIOThrows st action = do
  (res, newSt) <- flip runStateT st . runExceptT $ action
  pure (either show show res, newSt)

isBound :: String -> Env -> Bool
isBound var Env {..} = var `M.member` dict

getVar :: String -> Env -> StateThrowsError LispVal
getVar var Env{..} = do
  maybe
    (throwError $ UnboundVar "Getting an unbound variable" var)
    pure
    (M.lookup var dict)

setVar :: Env -> String -> LispVal -> StateThrowsError LispVal
setVar envRef var value = do
  unless (var `isBound` envRef) $ throwError $ UnboundVar "Setting an unbound variable" var
  _dict Lens.%= M.insert var value
  pure value

defineVar :: Env -> String -> LispVal -> Env
defineVar envRef var value = envRef Lens.&
  _dict Lens.%~ M.insert var value
  Lens.&
  _defined Lens.%~ Set.insert var

-- Creates new frame by erasing previous bindings
bindVars :: Env -> [(String, LispVal)] -> Env
bindVars envRef bindings = envRef Lens.&
  _dict Lens.%~ M.union (M.fromList bindings)
  Lens.&
  _defined Lens..~ vars
  where
    vars = Set.fromList $ fst <$> bindings

mergeEnvs :: Env -> Env -> Env
mergeEnvs oldEnv newEnv = oldEnv Lens.&
  _dict Lens.%~ M.union oldBindings
  where
    oldBindings = M.restrictKeys (dict newEnv) (defined oldEnv Set.\\ defined newEnv)

mergeClosures :: Env -> Env -> Env
mergeClosures oldClosure newClosure = oldClosure Lens.&
  _dict Lens.%~ M.union oldBindings
  where
    oldBindings = M.restrictKeys (dict newClosure) (defined oldClosure)
