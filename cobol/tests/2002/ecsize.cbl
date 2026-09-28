identification division.
program-id. ecsize.
*> EC-SIZE-TRUNCATION (COBOL 2002 14.7.5; cobol ISSUES-55): with checking
*> on and no ON SIZE ERROR phrase, a result too large for its receiver
*> raises it; the receiver keeps its value, the NOT ON SIZE ERROR phrase
*> is ignored, and the condition being fatal the run ends after the
*> declarative.  Before the TURN, the overflow is unchecked, and an ON SIZE
*> ERROR phrase always handles its own statement.  No oracle (ecraise).
data division.
working-storage section.
01  a        pic 99 value 90.
01  b        pic 99 value 0.
01  c        pic 99.
procedure division.
declaratives.
szd section.
    use after exception condition ec-size.
s1.
    display "declarative: " function exception-status " c=" c.
end declaratives.
main section.
m1.
    add 5 to a giving c
    display "no error: c=" c
    add 50 to a giving c
    display "unchecked overflow: c=" c
    add 50 to a giving c on size error display "the phrase handles it" end-add
>>TURN EC-SIZE CHECKING ON
    move 7 to c
    add 50 to a giving c not on size error display "not reached" end-add
    display "not reached either"
    stop run.
end program ecsize.
