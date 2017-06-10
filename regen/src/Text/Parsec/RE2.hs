{-# LANGUAGE NoMonomorphismRestriction #-}
module Text.Parsec.RE2 where

import Text.Parsec
import Control.Monad.Logic.RE2 (Expr)

-- 	Repetitions
-- x*	zero or more x, prefer more
-- x+	one or more x, prefer more
-- x?	zero or one x, prefer one
-- x{n,m}	n or n+1 or ... or m x, prefer more
-- x{n,}	n or more x, prefer more
-- x{,m}	m or less, prefer more
-- x{n}	exactly n x
data RepOp = ZeroOrMore | OneOrMore | ZeroOrOne | FromNtoM String String | NorMore String | Exactly String | NoOp deriving Show
repOp = option NoOp (choice [zeroOrMore, oneOrMore, zeroOrOne, range])
  where
    zeroOrMore = do
      char '*'
      optional (char '?' <|> char '+')
      return ZeroOrMore
    oneOrMore = do
      char '+'
      optional (char '?' <|> char '+')
      return OneOrMore
    zeroOrOne = do
      char '?'
      optional (char '?' <|> char '+')
      return ZeroOrOne
    range = do
      char '{'
      rop <- rangeOp
      char '}'
      return rop
    rangeOp = do
      from <- many1 digit
      option (Exactly from) (do{ char ','; option (NorMore from) (do{ to <- many1 digit; return (FromNtoM from to)})})


-- x	single character
-- A-Z	character range (inclusive)
-- \d	Perl character class
-- [:foo:]	ASCII character class foo
-- \p{Foo}	Unicode character class Foo
-- \pF	Unicode character class F (one-letter name)
data CharExpr = LiteralChar Char | AnyChar | CharClass [ClassExpr] deriving Show
data ClassExpr = LiteralClassExpr Char | RangeClassExpr Char Char deriving Show
charExpr = choice [literalChar, anyChar, charClass]
  where
    literalChar = do
      lc <- alphaNum
      return (LiteralChar lc)
    anyChar = do
      char '.'
      return AnyChar
    charClass = do
      char '['
      cc <- many1 classExpr
      char ']'
      return (CharClass cc)
    classExpr = do
      fromChar <- alphaNum
      option (LiteralClassExpr fromChar) (do{ char '-'; toChar <- alphaNum; return (RangeClassExpr fromChar toChar)})

-- (re)	numbered capturing group (submatch)
-- (?P<name>re)	named & numbered capturing group (submatch)
-- (?:re)	non-capturing group
-- (?flags:re)	set flags during re; non-capturing
-- (?flags)	set flags within current group; non-capturing
data GroupExpr = Group Expr deriving Show
groupExpr = do
  char '('
  optional (do
    char '?'
    choice [groupHeader1, groupHeader2])
  e <- expr
  char ')'
  return $ Group e
  where
    groupHeader1 = do
      char 'P'
      char '<'
      groupName
      char '>'
    groupHeader2 = do
      fs <- flags
      char ':'
    flags = many (choice [char 'i', char 'm', char 's', char 'U'])
    groupName = many1 (alphaNum <|> char '_')


data Object = CharObj CharExpr RepOp | GroupObj GroupExpr RepOp deriving Show
objExpr = choice [groupObj, charObj]
  where
    groupObj = do
      ge <- groupExpr
      r <- repOp
      return $ GroupObj ge r
    charObj = do
      ce <- charExpr
      r <- repOp
      return $ CharObj ce r

data Conjuction = Conj [Object] deriving Show
conjExpr = do
  objects <- many1 objExpr
  return $ Conj objects

data Disjunction = Disj [Conjuction] deriving Show
disjExpr = do
  conjs <- conjExpr `sepBy` char '|'
  return $ Disj conjs

expr :: ParsecT Expr
expr = disjExpr
