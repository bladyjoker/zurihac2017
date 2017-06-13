module Control.Monad.Logic.RE2 where

import Control.Applicative
import Control.Monad
import Control.Monad.Logic (runLogic, (>>-), interleave, Logic)

-- Enumerate sentences given a grammar G

type Sentence = String

data Expr = And Expr Expr
  | Or Expr Expr
  | Literal !Char
  | Empty
  | Group Expr
  | ZeroOrMore Expr
  | OneOrMore Expr
  | ZeroOrOne Expr
  | FromNtoM !Int !Int Expr
  | NorMore !Int Expr
  | NorLess !Int Expr
  | Exactly !Int Expr
  deriving Show

orL :: Logic Sentence -> Logic Sentence -> Logic Sentence
orL = interleave
andL :: Logic Sentence -> Logic Sentence -> Logic Sentence
--andL lx ly = lx >>- (\x -> (ly >>- (\y -> return $ x ++ y)))
andL lx ly = pure (++) <*> lx <*> ly

runRE2 expr = runLogic (logic expr) (:) []

logic :: Expr -> Logic Sentence
logic (And l r) = logic l `andL` logic r
logic (Or l r) = logic l `orL` logic r
logic (Literal c) = return [c]
logic Empty = return ""

logic (OneOrMore expr) = logic $ Or expr (And expr (OneOrMore expr)) -- <expr>+ == <expr>|<expr><expr>+

logic (Group expr) = logic expr -- (<expr>) == <expr>

logic (ZeroOrOne expr) = logic $ Or Empty expr -- <expr>? == <empty>|<expr>

logic (ZeroOrMore expr) =  logic $
  Or (ZeroOrOne expr) (And expr (OneOrMore expr)) -- <expr>* == <empty>?|<expr><expr>+

logic (Exactly n expr) =  logic $
  if n > 0
  then And expr (Exactly (n-1) expr)
  else Empty -- <expr>{n},n>0 == <expr><expr>{n-1} <expr>{0} == <empty>

logic (FromNtoM n m expr) =  logic $
  if n < m
  then Or (Exactly n expr) (FromNtoM (n+1) m expr)
  else Exactly m expr -- <expr>{n,m},n<m == <expr>{n}|<expr>{n+1,m} <expr>{m,m} == <expr>{m}

logic (NorMore n expr) = logic $
  Or (Exactly n expr) (NorMore (n+1) expr) -- <expr>{n,} == <expr>{n} | <expr>{n+1,}

logic (NorLess n expr) = logic $
  if n > 0
  then Or (Exactly n expr) (NorLess (n-1) expr)
  else Empty
