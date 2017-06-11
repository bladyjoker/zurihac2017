{-# LANGUAGE NoMonomorphismRestriction #-}
module Text.Parsec.RE2 where

import Text.Parsec
import Control.Monad.Logic.RE2 as RE2

-- a - Literal 'a'
-- [a-z0-9!@] - Range 'a' 'z'
-- (a) - Group (Literal 'a')
-- a* - ZeroOrMore (Literal 'a')
-- a+ - OneOrMore (Literal 'a')
-- a? - ZeroOrOne (Literal 'a')
-- a{1,3} - FromNtoM 1 3 (Literal 'a')
-- a|b - Or [Literal 'a', Literal 'b']
-- ab - And [Literal 'a', Literal 'b']

charExpr = choice [literalChar, anyChar, charClass]
  where
    literalChar = do
      lc <- alphaNum
      return (RE2.Literal lc)
    anyChar = do
      char '.'
      return $ RE2.Range '\0' '\255'
    charClass = do
      char '['
      classes <- many1 classExpr
      char ']'
      return (RE2.Or classes)
    classExpr = do
      fromChar <- alphaNum
      option (RE2.Literal fromChar) (do { char '-'; toChar <- alphaNum; return (RE2.Range fromChar toChar)})

integer = fmap read $ many1 digit

repOp expr = option expr (choice [zeroOrMore, oneOrMore, zeroOrOne, range])
  where
    zeroOrMore = do
      char '*'
      optional (char '?' <|> char '+')
      return $ ZeroOrMore expr
    oneOrMore = do
      char '+'
      optional (char '?' <|> char '+')
      return $ OneOrMore expr
    zeroOrOne = do
      char '?'
      optional (char '?' <|> char '+')
      return $ ZeroOrOne expr
    range = do
      char '{'
      rop <- rangeOp
      char '}'
      return rop
    rangeOp = do
      from <- integer
      option (Exactly from expr) (do{ char ','; option (NorMore from expr) (do{ to <- integer; return (FromNtoM from to expr)})})

groupExpr = do
  char '('
  optional (do
    char '?'
    choice [groupHeader1, groupHeader2])
  e <- expr
  char ')'
  return $ RE2.Group e
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

objExpr = choice [groupObj, charObj]
  where
    groupObj = do
      ge <- groupExpr
      repOp ge
    charObj = do
      ce <- charExpr
      repOp ce

conjExpr = do
  objects <- many1 objExpr
  return $ And objects

disjExpr = do
  conjs <- conjExpr `sepBy` char '|'
  return $ Or conjs

expr = disjExpr

parseRE2 = parse expr ""

-- import Text.Parsec
-- parse charExpr "" "[aba-z]"
-- parse (repOp $ Literal 'a') "" "+"
-- parse (groupExpr) "" "(abcd)"
