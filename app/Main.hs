module Main where

import REPL (runOne, runRepl)
import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (Parser)
import qualified Text.ParserCombinators.Parsec as P

main :: IO ()
main = do
  args <- getArgs
  if null args
    then runRepl
    else runOne args
