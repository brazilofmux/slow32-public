*> sub5 and sub6 of 2002/ecraising, built as separate programs under
*> >>PROPAGATE ON (7.3.21: outside a compilation unit, so a file of its
*> own): a condition nothing in the program handles is handed to the
*> caller as if by GOBACK RAISING LAST EXCEPTION; a program that
*> turns no checking on propagates nothing (no condition exists); sub6
*> comes first, as a >>TURN applies to all the text after it (7.3.25).
>>PROPAGATE ON
identification division.
program-id. sub6.
data division.
working-storage section.
01  a pic 9 value 1.
01  z pic 9 value 0.
procedure division.
    display "in sub6"
    divide a by z giving a
    display "after the divide in sub6 (no checking on)"
    goback.
end program sub6.
identification division.
program-id. sub5.
data division.
working-storage section.
01  a pic 9 value 1.
01  z pic 9 value 0.
procedure division.
>>TURN EC-SIZE-ZERO-DIVIDE CHECKING ON
    display "in sub5"
    divide a by z giving a
    display "after the divide (not reached)"
    goback.
end program sub5.
