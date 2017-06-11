module Main where

import System.Environment (getArgs)
import Control.Monad.Logic
import Control.Monad.Logic.RE2
import Text.Parsec.Test

main = do
  args <- getArgs
  mapM putStrLn args
  --runLogic (app (FromNtoM 1 3 (CharC (Literal 'a') []))) (:) []
  mapM (\s -> do putStrLn s; putStrLn "------------") (take 500 $ runLogic (app $ Or [astar, bstar, cstar]) (:) [])

astar = ZeroOrMore (Literal 'a')
bstar = ZeroOrMore (Literal 'b')
cstar = ZeroOrMore (Literal 'c')
