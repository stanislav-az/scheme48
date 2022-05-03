module REPL where

import Control.Monad ((<=<), unless)
import Control.Monad.Error.Class (liftEither)
import Lisp
import System.IO (hFlush, hPutStrLn, stderr, stdout)

runRepl :: IO ()
runRepl =
  primitiveBindings >>=
  until_ (== "quit") (readPrompt "Lisp-lazy>>> ") . evalAndPrint

runOne :: [String] -> IO ()
runOne args = do
  env <-
    primitiveBindings >>=
    flip bindVars [("args", List $ map String $ drop 1 args)]
  runIOThrows (actualValue env (List [Atom "load", String (head args)])) >>=
    hPutStrLn stderr

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftEither (readExpr expr) >>= actualValue env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env = putStrLn <=< evalString env

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  unless (pred result) $ action result >> until_ pred prompt action
