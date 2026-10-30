* Statements: BEGIN SEQUENCE, BREAK with a value, RECOVER USING
LOCAL x
BEGIN SEQUENCE
   ? "before"
   BREAK "the value"
   ? "not here"
RECOVER USING x
   ? "recovered", x
END
? "after"
