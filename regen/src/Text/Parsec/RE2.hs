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

-- a - Literal 'a'
-- . - Range '\0' '\255'
-- [a-z!0-9] - Or [Range 'a' 'z', Literal '!', Range '0' '9']
-- TODO: \.
-- TODO: \[ \]
-- TODO: [\-]
charExpr = choice [literalChar, anyChar, charClass]
  where
    literalChar = do
      lc <- alphaNum
      return (RE2.Literal lc)
    anyChar = do
      char '.'
      ascii <- range '\0' '\255'
      return ascii
    charClass = do
      char '['
      classes <- many1 classExpr
      char ']'
      case classes of
        [] -> error "Empty character class."
        [c] -> return c
        (c:cs) -> return $ foldl RE2.Or c cs
    classExpr = do
      fromChar <- alphaNum
      option
        (RE2.Literal fromChar)
        (do
            char '-'
            toChar <- alphaNum
            r <- range fromChar toChar
            return r
         )
    range from to = case enumFromTo from to of
      [] -> error "Invalid range."
      [c] -> return $ Literal c
      (c:cs) -> return $ foldl Or (Literal c) (fmap Literal cs)

integer = do
  digits <- many1 digit
  case reads digits :: [(Int, String)] of
    [(int, _)] -> return int
    _ -> error "Failed to parse integer." -- this should never happen.

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
    rangeOp = atleastn <|> norless
    atleastn = do
      from <- integer
      option
        (Exactly from expr)
        (do
            char ','
            option
              (NorMore from expr)
              (do
                  to <- integer
                  return (FromNtoM from to expr)
              )
        )
    norless = do
      char ','
      to <- integer
      return $ NorLess to expr

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

objExpr = do
  objL <- choice [groupObj, charObj]
  option
    objL
    (do
       objR <- objExpr
       return $ And objL objR
        )
  where
    groupObj = do
      ge <- groupExpr
      repOp ge
    charObj = do
      ce <- charExpr
      repOp ce

disjExpr = do
  objL <- objExpr
  option
    objL
    (do
       char '|'
       disjR <- disjExpr
       return $ Or objL disjR
     )

expr = disjExpr

parseRE2 = parse expr ""

-- import Text.Parsec
-- parse charExpr "" "[aba-z]"
-- parse (repOp $ Literal 'a') "" "+"
-- parse (groupExpr) "" "(abcd)"
