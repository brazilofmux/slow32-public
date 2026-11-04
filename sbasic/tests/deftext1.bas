' DEFtype is the text's (QuickBASIC's): a name with no suffix takes the
' DEFtype in force where it is written -- x before DEFINT is x!, after it
' x%, two variables; x! stays a SINGLE; arrays, DEF FN (by its letter
' after FN) and their parameters too; a CONST keeps its value's type
x = 2.7: PRINT x
DEFINT A-Z
x = 3.7: PRINT x; x!; x%
x! = 1.5: PRINT x; x!
DIM a(3): a(1) = 2.6: PRINT a(1); a%(1)
DEF FNb (c) = c * 1.5: PRINT FNb(3)
CONST k = 2.5: PRINT k
FOR i = 1 TO 2: s = s + i / 2: NEXT i: PRINT s; i
DEFDBL D
d = 1 / 3: PRINT d; d#
