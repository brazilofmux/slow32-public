identification division.
program-id. ecsizeexp.
*> Exponentiation's own size errors (2023 8.8.1.2 rule 6): zero to a power
*> not above zero, and a negative base to a non-integer power, raise
*> EC-SIZE-EXPONENTIATION -- not EC-SIZE-OVERFLOW, which is what the stack
*> reported before 2026-09-30.  The ON SIZE ERROR phrase handles each;
*> with checking on and no phrase the declarative sees the name, and the
*> condition being fatal the run ends after it.  No oracle (ecraise).
data division.
working-storage section.
01  z        pic 9 value 0.
01  m        pic s9 value -8.
01  r        pic s9(5)v99 value 7.
procedure division.
declaratives.
szd section.
    use after exception condition ec-size.
s1.
    display "declarative: " function exception-status " r=" r.
end declaratives.
main section.
m1.
    compute r = z ** 0 on size error display "0 ** 0: size error" end-compute
    compute r = z ** -2 on size error display "0 ** -2: size error" end-compute
    compute r = m ** 0.5 on size error display "-8 ** 0.5: size error" end-compute
    compute r = m ** (1 / 3) on size error display "-8 ** (1/3): size error" end-compute
    compute r = 2 ** 3 ** 2 display "2 ** 3 ** 2 = " r
>>TURN EC-SIZE-EXPONENTIATION CHECKING ON
    compute r = m ** 0.5
    display "not reached"
    stop run.
end program ecsizeexp.
