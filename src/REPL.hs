module REPL where

import Control.Monad ((<=<), unless)
import Control.Monad.Error.Class (liftEither)
import Lisp
import System.IO (hFlush, hPutStrLn, stderr, stdout)

runRepl :: IO ()
runRepl =
  untilSt (== "quit") (readPrompt "Lisp>>> ") evalAndPrint primitiveBindings

runOne :: [String] -> IO ()
runOne args = do
  let env = bindVars primitiveBindings [("args", List $ map String $ drop 1 args)]
  runIOThrows env (eval (List [Atom "load", String (head args)])) >>=
    hPutStrLn stderr . fst

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO (String, Env)
evalString env expr = runIOThrows env $ liftEither (readExpr expr) >>= eval

evalAndPrint :: Env -> String -> IO Env
evalAndPrint env input = do
  (msg, st) <- evalString env input
  putStrLn msg
  pure st

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  unless (pred result) $ action result >> until_ pred prompt action

untilSt :: Monad m => (a -> Bool) -> m a -> (s -> a -> m s) -> s -> m ()
untilSt pred prompt action st = do
  result <- prompt
  unless (pred result) $ action st result >>= untilSt pred prompt action
