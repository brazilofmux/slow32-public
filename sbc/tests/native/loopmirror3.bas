' a jump before the DIM: the loop after it cannot know the DIM ran, and a
' program that skips it has the array made implicitly (10 elements), as
' QBasic's does for a dynamic one
DEFINT A-Z
n = 3
IF n = 3 THEN GOTO past
n = 20
past:
FOR i = 1 TO 3: PRINT i;: NEXT
PRINT
DIM d(n)
FOR i = 0 TO n: d(i) = i + 1: NEXT
PRINT d(n); UBOUND(d)
