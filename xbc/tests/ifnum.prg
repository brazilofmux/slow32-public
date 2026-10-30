* Interpreter IF requires VAL_LOGIC; a number is not true.
IF 1
  ? "taken"
ELSE
  ? "skipped"
ENDIF
IF .T.
  ? "logic-t"
ENDIF
RETURN
