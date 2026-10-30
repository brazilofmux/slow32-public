* DO of a procedure that was inlined via SET PROCEDURE.
* Pre-fix this regressed to "File not found" because SET PROCEDURE TO is
* dropped when inlined and a bare DO was not lowered to the UDF registry.
SET TALK OFF
SET PROCEDURE TO doproclib
? "before"
DO greet
DO SHOUT
? "after"
RETURN
