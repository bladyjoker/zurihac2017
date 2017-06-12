module Control.Monad.Logic.RE2 where

import Control.Monad.Logic
import Control.Monad
import Control.Applicative

-- Enumerate sentences given a grammar G

type Sentence = String

data Expr = And [Expr]
  | Or [Expr]
  | Literal !Char
  | Empty
  | Range !Char !Char
  | Group Expr
  | ZeroOrMore Expr
  | OneOrMore Expr
  | ZeroOrOne Expr
  | FromNtoM !Int !Int Expr
  | NorMore !Int Expr
  | NorLess !Int Expr
  | Exactly !Int Expr
  deriving Show

app :: Expr -> Logic Sentence
-- bug: Empty should not be a part of Or And
app (And exprs) = foldr andL (app Empty) (fmap app exprs)
app (Or exprs) = foldr orL (app Empty) (fmap app exprs)
-- app (And es) = foldr andL (app $ head es) (fmap app $ tail es)
-- app (Or es) = foldr orL (app $ head es) (fmap app $ tail es)

app (Literal c) = return [c]
app Empty = return ""

app (Range from to) = app $ Or (fmap Literal $ enumFromTo from to)

app (OneOrMore expr) = app $ Or [expr, (And [expr, OneOrMore expr])] -- <expr>+ == <expr>|<expr><expr>+

app (Group expr) = app expr -- (<expr>) == <expr>

app (ZeroOrOne expr) = app $ Or [Empty, expr] -- <expr>? == <empty>|<expr>

app (ZeroOrMore expr) =  app $
  Or [ZeroOrOne expr, (And [expr, OneOrMore expr])] -- <expr>* == <empty>?|<expr><expr>+

app (Exactly n expr) =  app $
  if n > 0
  then And [expr, Exactly (n-1) expr]
  else Empty -- <expr>{n},n>0 == <expr><expr>{n-1} <expr>{0} == <empty>

app (FromNtoM n m expr) =  app $
  if n < m
  then Or [Exactly n expr, FromNtoM (n+1) m expr]
  else Exactly m expr -- <expr>{n,m},n<m == <expr>{n}|<expr>{n+1,m} <expr>{m,m} == <expr>{m}

app (NorMore n expr) = app $
  Or [Exactly n expr, NorMore (n+1) expr] -- <expr>{n,} == <expr>{n} | <expr>{n+1,}

app (NorLess n expr) = app $
  if n > 0
  then Or [Exactly n expr, NorLess (n-1) expr]
  else Empty

orL :: Logic Sentence -> Logic Sentence -> Logic Sentence
orL = interleave
andL :: Logic Sentence -> Logic Sentence -> Logic Sentence
andL lx ly = lx >>- (\x -> (ly >>- (\y -> return $ x ++ y)))

runRE2 expr = runLogic (app expr) (:) []
