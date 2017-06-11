module Main where

import System.Environment (getArgs)
import Control.Monad.Logic
import Control.Monad.Logic.RE2
import Text.Parsec.RE2 (parseRE2)

parseAndGenerate string = do
  re2 <- parseRE2 string
  return $ runRE2 re2

output (Left e) = mapM putStrLn [show e]
output (Right sentences) = mapM putStrLn sentences

main = do
  args <- getArgs
  mapM (output . parseAndGenerate) args
