REM a DEF FN's type is the letter after FN's (checked against QBasic 1.1)
DEFSTR S
DEFINT F
DEF FNS(X$) = X$ + "!"
DEF FNA(X) = X * 1.6
PRINT FNS("hi"); FNA(1)
