identification division.
program-id. p-std2023-call-overflow.
*> COBOL 2023 removed CALL ... ON OVERFLOW (Annex E.2 item 1; BP-R2).
data division.
working-storage section.
01 x pic x.
procedure division.
    call "nobody" on overflow display "x" end-call
    stop run.
