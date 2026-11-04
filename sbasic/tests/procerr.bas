' errors in SUBs and FUNCTIONs, as QBasic traps them: the handler is the
' main program's (its label there), set in a SUB or out of one; RESUME
' NEXT and RESUME return into the procedure, to the statement after or
' the statement again; a FOR or a GOSUB in the procedure goes on; a
' FUNCTION's error; procedures two deep; RESUME label leaves them all;
' COMMON SHARED's variables are every procedure's -- the trail printed
DECLARE SUB arm ()
DECLARE SUB loopy ()
DECLARE SUB retry ()
DECLARE SUB outer ()
DECLARE SUB inner ()
DECLARE SUB withsub ()
DECLARE SUB leave ()
DECLARE FUNCTION half! (n!)
COMMON SHARED k$, d, tries
k$ = "<"
arm
k$ = k$ + "A"
loopy
retry
outer
withsub
v = half(0): k$ = k$ + STR$(v) + "|"
v = half(8): k$ = k$ + STR$(v) + "|"
ON ERROR GOTO unwind
FOR j = 1 TO 3
  k$ = k$ + "j" + MID$(STR$(j), 2)
  IF j = 2 THEN leave
NEXT
k$ = k$ + ">"
PRINT k$
END
mend:
k$ = k$ + "(" + MID$(STR$(ERR), 2) + ")"
IF ERR = 11 THEN d = 4: RESUME
RESUME NEXT
unwind:
k$ = k$ + "{" + MID$(STR$(ERR), 2) + "}"
RESUME onward
onward:
k$ = k$ + "!" + ">"
PRINT k$
END
SUB arm
ON ERROR GOTO mend
k$ = k$ + "a"
ERROR 52
k$ = k$ + "b"
END SUB
SUB loopy
FOR i = 1 TO 3
  k$ = k$ + MID$(STR$(i), 2)
  IF i = 2 THEN x = SQR(-1)
  k$ = k$ + "."
NEXT
END SUB
SUB retry STATIC
d = 0
q = 12 / d
k$ = k$ + "r" + MID$(STR$(q), 2)
END SUB
SUB outer
k$ = k$ + "o"
inner
k$ = k$ + "O"
END SUB
SUB inner
DIM v(2)
v(5) = 1
k$ = k$ + "i"
END SUB
SUB withsub
GOSUB part
k$ = k$ + "w"
EXIT SUB
part:
k$ = k$ + "p"
ERROR 61
k$ = k$ + "P"
RETURN
END SUB
FUNCTION half! (n!)
half = 0
k$ = k$ + "h"
z = LOG(n!)
half = n! / 2
END FUNCTION
SUB leave
k$ = k$ + "l"
ERROR 70
k$ = k$ + "L"
END SUB
