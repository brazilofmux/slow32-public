identification division.
program-id. x6202.
*> No figurative constant with ALL as an operand of & (rule 1).
procedure division.
    display "a" & all "b"
    stop run.
