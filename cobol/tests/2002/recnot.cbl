identification division.
program-id. recnot.
*> A program that is not RECURSIVE, called while it is active: the
*> EC-PROGRAM-RECURSIVE-CALL condition, fatal (2023 14.9.4 general rule
*> 3f; cobol ISSUES-49).  The run stops at the second entry, so the line
*> after the CALL never prints.  GnuCOBOL stops there too, with its own
*> message on stderr.
data division.
working-storage section.
01  times-in pic 9 value 0.
procedure division.
main.
    add 1 to times-in
    display "entry " times-in
    if times-in < 3
        call "recnot"
    end-if
    display "not reached"
    stop run.
end program recnot.
