* Nested DO must not smash caller IF/CASE/loop
? "--- nested DO CASE ---"
DO CASE
  CASE .T.
    DO nested_case
    ? "CALLER-CASE-CONT"
  OTHERWISE
    ? "CALLER-OTHERWISE-BAD"
ENDCASE
? "CALLER-AFTER-CASE"

? "--- nested IF ---"
IF .T.
  DO nested_if
  ? "CALLER-IF-CONT"
ELSE
  ? "CALLER-ELSE-BAD"
ENDIF
? "CALLER-AFTER-IF"

? "--- nested DO WHILE ---"
i = 1
DO WHILE i <= 2
  ? "CALLER-WHILE", i
  DO nested_loop
  i = i + 1
ENDDO
? "CALLER-AFTER-WHILE"
RETURN

PROCEDURE nested_case
  DO CASE
    CASE .T.
      ? "CALLEE-CASE"
    OTHERWISE
      ? "CALLEE-OTHERWISE-BAD"
  ENDCASE
  ? "CALLEE-AFTER-CASE"
RETURN

PROCEDURE nested_if
  IF .T.
    ? "CALLEE-IF"
  ELSE
    ? "CALLEE-ELSE-BAD"
  ENDIF
  ? "CALLEE-AFTER-IF"
RETURN

PROCEDURE nested_loop
  j = 1
  DO WHILE j <= 5
    ? "CALLEE-LOOP", j
    IF j = 1
      RETURN
    ENDIF
    j = j + 1
  ENDDO
  ? "CALLEE-LOOP-SHOULD-NOT"
RETURN
