' RESTORE in a SUB naming the main program's label; a FUNCTION v% named
' v; a SUB called mid-line before a colon: QBasic loads it
DECLARE FUNCTION v% ()
DECLARE SUB again ()
again: PRINT v
END
nums:
DATA 7
SUB again
  RESTORE nums
END SUB
FUNCTION v%
  READ v
END FUNCTION
