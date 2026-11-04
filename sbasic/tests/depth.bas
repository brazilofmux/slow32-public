REM === Recursion depth limit test ===

REM Normal recursion should work
SUB countdown(n)
  IF n > 0 THEN
    CALL countdown(n - 1)
  ELSE
    PRINT "reached bottom"
  END IF
END SUB
CALL countdown(10)

REM Excessive recursion: Out of stack space, which QBasic does not trap in
REM a procedure (the handler would need the stack) -- the program stops
ON ERROR GOTO trap
SUB infinite(n)
  CALL infinite(n + 1)
END SUB
CALL infinite(1)
PRINT "FAIL: should not reach"
END

trap:
PRINT "Caught depth error:"; ERR
PRINT "Done"
