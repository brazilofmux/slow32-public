' s$ + e, as QBasic evaluates it: s$ (a variable-length string variable)
' is read as the two are joined, after e -- so a FUNCTION in e that
' changes s$ is seen, s$ = s$ + f$ appending to s$ as f$ leaves it.
' Not so for s$ + "-" + f$ (s$ + "-" made first), an array's element,
' a fixed-length string, or numbers
DECLARE FUNCTION tag$ ()
DECLARE FUNCTION mark$ ()
DECLARE FUNCTION bump% ()
DIM SHARED s$, list$(2), fx AS STRING * 5, n%
s$ = "Q": s$ = s$ + tag$: PRINT s$
s$ = "Q": s$ = s$ + ":" + tag$: PRINT s$
s$ = "Q": s$ = s$ + tag$ + ":": PRINT s$
s$ = "Q": s$ = (s$) + (tag$): PRINT s$
s$ = "Q": s$ = s$ + UCASE$(tag$): PRINT s$
s$ = "Q": s$ = s$ + s$ + tag$: PRINT s$
s$ = "Q": s$ = tag$ + s$: PRINT s$
s$ = "Q": u$ = s$ + tag$: PRINT u$; " "; s$
s$ = "Q": u$ = s$ + ":" + tag$: PRINT u$; " "; s$
list$(1) = "Q": list$(1) = list$(1) + mark$: PRINT list$(1)
fx = "Q": fx = fx + mark$: PRINT fx; "]"
n% = 1: n% = n% + bump%: PRINT n%
s$ = "Q": PRINT s$; tag$; s$
FUNCTION tag$
s$ = s$ + "z"
tag$ = "T"
END FUNCTION
FUNCTION mark$
list$(1) = list$(1) + "m"
fx = "WW"
mark$ = "M"
END FUNCTION
FUNCTION bump%
n% = n% + 50
bump% = 7
END FUNCTION
