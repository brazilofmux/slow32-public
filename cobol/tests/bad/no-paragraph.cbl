identification division.
program-id. nopara.
*> PERFORM and GO TO naming a paragraph that does not exist say so
*> (cobol ISSUES-41; they used to read "is not a COBOL verb" and "GO TO
*> without a procedure-name").  PERFORM n TIMES is still a loop.
data division.
working-storage section.
01  n pic 9 value 2.
procedure division.
main.
    perform missing-para.
    perform n times display "x" end-perform.
    go to also-missing.
    stop run.
