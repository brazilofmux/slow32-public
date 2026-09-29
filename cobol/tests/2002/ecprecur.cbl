identification division.
program-id. ecprecur recursive.
*> An exception-checking PERFORM in a RECURSIVE program (cobol ISSUES-94
*> E9): a WHEN phrase that calls its own program has its resume point on
*> libcob's stack, not in a static word, so the inner activation's raise
*> does not overwrite the outer's -- depth 1 resumes after its own
*> raising statement ("between the statements"). The raise inside the IF
*> resumes after the IF, the statement of imperative-statement-1 it is in
*> (2023 14.9.28 rule 20).
*> No oracle: GnuCOBOL 4 has no exception-checking PERFORM.
data division.
working-storage section.
01 depth pic 9 value 0.
local-storage section.
01 me pic 9.
procedure division.
m1.
    add 1 to depth
    move depth to me
    perform
        if me = 1
            raise exception ec-user-a
            display "d1: not expected (inside the IF)"
        else
            display "  d2: raise in 2nd statement"
            continue
        end-if
        display "  depth " me ": between the statements"
        if me = 2
            raise exception ec-user-a
            display "d2: not expected (inside the IF)"
        end-if
    when exception ec-user-a
        display "  WHEN in depth " me
        if me = 1
            call "ecprecur"
        end-if
    end-perform
    display "end of depth " me
    goback.
