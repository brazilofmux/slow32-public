identification division.
program-id. cancelactive.
*> CANCEL of a program that is active (2023 14.9.5.4 rule 5): it is not
*> canceled.  Unchecked, the result is the implementor's, and here the
*> CANCEL does nothing -- before the sweep of 2026-09-30 it reset the
*> WORKING-STORAGE of the running program.  Checked, EC-PROGRAM-CANCEL-
*> ACTIVE is raised in the program holding the CANCEL; it is fatal, so the
*> run ends after the declarative.  No oracle: GnuCOBOL stops the run
*> with a message of its own in both cases.
procedure division.
    call "selfc"
    display "back in main"
    call "selfk"
    display "not reached"
    stop run.
end program cancelactive.
identification division.
program-id. selfc.
data division.
working-storage section.
01 n pic 9 value 5.
procedure division.
    move 7 to n
    cancel "selfc"
    display "selfc, unchecked: not canceled, n=" n
    goback.
end program selfc.
identification division.
program-id. selfk.
procedure division.
declaratives.
dk section.
    use after exception condition ec-program-cancel-active.
d1.
    display "declarative: " function exception-status.
end declaratives.
main section.
m1.
>>TURN EC-PROGRAM-CANCEL-ACTIVE CHECKING ON
    cancel "selfk"
    display "not reached"
    goback.
end program selfk.
