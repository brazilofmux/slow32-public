identification division.
program-id. ecraise.
*> RAISE, USE AFTER EXCEPTION CONDITION and the last exception status
*> (COBOL 2002 14.6.13, 14.9.29, 14.9.49 format 3; cobol ISSUES-53).
*> A RAISE before >>TURN turns checking on does nothing (7.3.25 rule 1:
*> checking is off by default).  A nonfatal condition runs its declarative
*> and goes on; a fatal one runs its declarative -- here EC-SIZE's, the
*> group of EC-SIZE-OVERFLOW -- and ends the run (14.6.13.1.3 rule 5), so
*> the last line never prints.  No oracle: GnuCOBOL 4 does not implement
*> USE AFTER EXCEPTION CONDITION, and runs past a fatal RAISE.
procedure division.
declaratives.
user-x section.
    use after exception condition ec-user-x.
ux.
    display "declarative for " function exception-status
    display "statement [" function exception-statement "]".
size-any section.
    use after ec ec-size.
sz.
    display "size group declarative: " function exception-status.
end declaratives.
main section.
m1.
    display "status before: [" function exception-status "]"
    raise exception ec-user-x
    display "not turned on: nothing ran"
>>TURN EC-USER-X CHECKING ON WITH LOCATION
    raise exception ec-user-x
    display "after the nonfatal raise"
    set last exception to off
    display "cleared: [" function exception-status "]"
>>TURN EC-SIZE CHECKING ON
    raise exception ec-size-overflow
    display "not reached: EC-SIZE-OVERFLOW is fatal"
    stop run.
end program ecraise.
