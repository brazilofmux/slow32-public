identification division.
program-id. exout.
*> EXIT PERFORM is only in an inline or exception-checking PERFORM (2023 14.9.14.3 rule 8).
procedure division.
    exit perform
    stop run.
