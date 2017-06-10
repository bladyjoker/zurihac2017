module Main where

import Control.Monad.Logic


main = undefined

a = [1,2..]

b = fmap (*10) a

test = interleave a a
test2 = join [a, a]
test3 = sequence [a,b]

test4 = do
  x <- a
  y <- b
  return (a,b)

--test5 :: Logic (Int, Int)
test5 = a >>- (\x -> (b >>- (\y -> return (x,y))))

test6 :: Logic Int
test6 = do
  x <- msum (fmap return a)
  guard (even x)
  return x

runlist :: [a] -> [a]
runlist = id

-- Enumerate sentences given a grammar G

type Sentence = String

char :: Char -> Logic Sentence
char c = return [c]

choice :: String -> Logic Sentence
choice cs = msum (fmap (return . (:[])) cs)

concat_ :: Logic Sentence -> Logic Sentence -> Logic Sentence
concat_ lx ly = lx >>- (\x -> (ly >>- (\y -> return $ x ++ y)))

kleen :: Logic Sentence -> Logic Sentence
kleen ls = mzero `mplus` ls `mplus` (ls `concat_` kleen ls)

-- IP 127.0.0.1
-- ([0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3})*

kleen1 = kleen (char 'a')
kleen2 = kleen (char 'b')

testkleen = kleen1 `concat_` kleen2
