' '$DYNAMIC and '$STATIC: how each DIM from there on, in the text's order,
' makes its array, whichever way the comment is written and whatever runs
ON ERROR GOTO h
DIM st0%(3)
st0%(1) = 5
ERASE st0%
PRINT "static zeroed"; st0%(1); UBOUND(st0%)
'$DYNAMIC
DIM dy1%(3)
dy1%(1) = 5
ERASE dy1%
PRINT "A";
PRINT dy1%(1)
REDIM dy1%(6): PRINT UBOUND(dy1%)
DIM dy1b%(2): REDIM dy1b%(4): PRINT UBOUND(dy1b%)
DIM twice%(1): DIM twice%(1): PRINT "after twice"
ERASE twice%: DIM twice%(7): PRINT UBOUND(twice%)
'$STATIC
DIM st1%(3)
st1%(1) = 2
ERASE st1%
PRINT "B"; st1%(1)
' $dynamic with a space and in lower case, then words after it
DIM dy2%(3)
ERASE dy2%
PRINT "C";
PRINT dy2%(1)
REM $STATIC
DIM st2%(3)
ERASE st2%
PRINT "D"; st2%(1)
REM $DYNAMIC and more
DIM dy3%(3)
ERASE dy3%
PRINT "E";
PRINT dy3%(1)
'$STATICally
DIM st3$(3)
st3$(1) = "x"
ERASE st3$
PRINT "F["; st3$(1); "]"
' not one: a space after the $, words before it, in a string
'$ DYNAMIC
DIM st4%(3)
ERASE st4%
PRINT "G"; st4%(1)
' x $DYNAMIC
PRINT "'$DYNAMIC"
DIM st5%(3)
ERASE st5%
PRINT "H"; st5%(1)
' on a DIM's own line: later than that DIM
DIM st6%(3) '$DYNAMIC
ERASE st6%
PRINT "I"; st6%(1)
DIM dy6%(3) '$STATIC
ERASE dy6%
PRINT "J";
PRINT dy6%(1)
' at the end of a line, in a branch not taken, past a jump
n = 1 '$DYNAMIC
DIM dy7%(3)
ERASE dy7%
PRINT "K";
PRINT dy7%(1)
'$STATIC
IF n = 0 THEN
    '$DYNAMIC
END IF
DIM dy8%(3)
ERASE dy8%
PRINT "L";
PRINT dy8%(1)
'$STATIC
GOTO past
'$DYNAMIC
past:
DIM dy9%(3)
ERASE dy9%
PRINT "M";
PRINT dy9%(1)
' an implicit array stays static; bounds not constant stay dynamic
ipl%(1) = 5
ERASE ipl%
PRINT "N"; ipl%(1); UBOUND(ipl%)
'$STATIC
k = 3
DIM va%(k)
ERASE va%
PRINT "O";
PRINT va%(1)
' records, SHARED, CLEAR's view, the bytes
TYPE rt
    x AS INTEGER
END TYPE
'$DYNAMIC
DIM rc(3) AS rt
ERASE rc
PRINT "P";
PRINT rc(1).x
DIM SHARED sh%(3)
ERASE sh%
PRINT "Q";
PRINT sh%(1)
DIM by%(3)
by%(1) = 258
DEF SEG = VARSEG(by%(0))
PRINT VARPTR(by%(1)); PEEK(VARPTR(by%(1)))
DEF SEG
DIM cl%(3)
CLEAR
ON ERROR GOTO h
PRINT "R";
PRINT cl%(1)
PRINT "done"
SYSTEM
h:
PRINT "err"; ERR
RESUME NEXT
