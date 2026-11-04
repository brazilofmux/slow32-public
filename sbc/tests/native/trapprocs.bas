' errors in procedures, compiled: each procedure's variables live on
' through the handler and RESUME back into it -- strings, a local array,
' a parameter by reference, a FOR's counter, a FUNCTION's value -- in
' recursion with the error deep down, and RESUME NEXT past the error
DECLARE SUB walk (n%, acc$)
DECLARE SUB grid (s%)
DECLARE FUNCTION tally& (m%)
DIM SHARED log$
ON ERROR GOTO handler
a$ = ""
walk 4, a$
log$ = log$ + "|" + a$
total% = 0
grid total%
log$ = log$ + "|" + STR$(total%)
v& = tally&(5)
log$ = log$ + "|" + STR$(v&)
PRINT log$
END
handler:
log$ = log$ + "<" + MID$(STR$(ERR), 2) + ">"
RESUME NEXT
SUB walk (n%, acc$)
DIM mine$(3)
mine$(1) = "w" + MID$(STR$(n%), 2)
here$ = "h" + MID$(STR$(n%), 2)
IF n% > 1 THEN walk n% - 1, acc$
IF n% = 2 THEN mine$(9) = "x"
acc$ = acc$ + mine$(1) + here$
END SUB
SUB grid (s%)
FOR r% = 1 TO 3
  FOR c% = 1 TO 3
    q% = 6 \ (r% - c%)
    s% = s% + q%
  NEXT
NEXT
END SUB
FUNCTION tally& (m%)
t& = 0
FOR i% = 1 TO m%
  IF i% = 3 THEN ERROR 200
  t& = t& + i% * 1000
NEXT
tally& = t&
END FUNCTION
