identification division.
program-id. ecovfl.
*> EC-SIZE-OVERFLOW (COBOL 2002 14.7.5 rule 3; cobol ISSUES-55): an
*> intermediate result past the 38 digits the wide stack holds
*> (docs/wide.md), with checking on through the EC-SIZE group.  The EC-ALL
*> declarative takes it, being the only one.  Fatal.  No oracle (ecraise).
*> It was big * big, 24 digits, when an 18-digit product overflowed the
*> narrow stack; that product is now computed wide and exact (tests/gen,
*> free/computewide), and only the store into r overflows -- a truncation,
*> not an overflow -- so the intermediate is four factors, 48 digits.
data division.
working-storage section.
01  big      pic 9(12) value 999999999999.
01  r        pic 9(18) value 5.
procedure division.
declaratives.
al section.
    use after exception condition ec-all.
a1.
    display "declarative: " function exception-status " r=" r.
end declaratives.
main section.
m1.
>>TURN EC-SIZE CHECKING ON
    compute r = big * 1000
    display "big * 1000 = " r
    compute r = big * big * big * big
    display "not reached"
    stop run.
end program ecovfl.
