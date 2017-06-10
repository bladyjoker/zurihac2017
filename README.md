# zurihac2017
ZuriHac2017 artifacts

## Regular expression sentence generator (regen)

Given a regular expression (https://github.com/google/re2) generate sentences captured by that language.
For instance:

```sh

$ regen a*

a
aa
aaa
aaaa
aaaaa
...

$ regen [a-z]+

a
b
c
d
e
f
g
h
...

```

The tool leverages:

 - https://hackage.haskell.org/package/logict for composing regular expressions, which are represented as logical statements, and finally generating sentences captured by the expression.
 - https://hackage.haskell.org/package/parsec for parsing the input string into a **LogicT Sentence**.
