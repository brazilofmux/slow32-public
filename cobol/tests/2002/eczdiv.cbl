identification division.
program-id. eczdiv.
*> EC-SIZE-ZERO-DIVIDE (COBOL 2002 14.7.5; cobol ISSUES-55): a zero
*> divisor with checking on, in a COMPUTE with no ON SIZE ERROR phrase.
*> Fatal: the declarative runs, then the run ends.  No oracle (ecraise).
data division.
working-storage section.
01  a        pic 9(4) value 100.
01  z        pic 9 value 0.
01  q        pic 9(4) value 42.
procedure division.
declaratives.
zd section.
    use after exception condition ec-size-zero-divide.
z1.
    display "declarative: " function exception-status " q=" q.
end declaratives.
main section.
m1.
>>TURN EC-SIZE-ZERO-DIVIDE CHECKING ON
    compute q = a / 4
    display "a / 4 = " q
    compute q = a / z
    display "not reached"
    stop run.
end program eczdiv.
