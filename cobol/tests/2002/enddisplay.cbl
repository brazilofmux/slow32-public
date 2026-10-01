*> END-DISPLAY (2023 14.9.11.2): every format of DISPLAY takes the scope
*> terminator.  It was never consumed, so the next statement found it
*> "without a matching statement" -- the one program of the GnuCOBOL
*> mirror in X-COBOL that did not compile.  UPON SYSERR goes to the
*> error stream; this checks only that what follows still runs.
identification division.
program-id. enddisplay.
data division.
working-storage section.
01 n pic 9 value 3.
procedure division.
    display "one"
    end-display
    display "two" with no advancing
    end-display
    display "/three"
    display "to stderr" upon syserr
    end-display
    if n > 2
        display "in an IF" end-display
        display "still in it"
    end-if
    display "done"
    stop run.
