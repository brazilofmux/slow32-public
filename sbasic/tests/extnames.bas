' variables named as the interpreter's own functions (MAX, MIN, ROUND):
' QuickBASIC has no such functions, so they are variables -- as QBasic
' 1.1 runs it
max = 3: bht = 2
IF max > bht THEN max = bht
round = 7
PRINT max; min; round
