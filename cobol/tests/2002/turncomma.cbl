*> A >>TURN after a numeric literal written with the decimal comma
*> (DECIMAL-POINT IS COMMA, 2023 12.3.7): the directive applies to the
*> statement right after it (7.3.4 rule 5).  The decimal-point pass joins
*> 45296,5 into one token after the positional directives were recorded
*> by token index, and every joined literal ahead of a >>TURN made it
*> apply one statement late (found by standard-queue item 27).
*> No oracle: GnuCOBOL 4 does not implement exception declaratives.
*> docs/conformance/turn.md
identification division.
program-id. turncomma.
environment division.
configuration section.
special-names.
    decimal-point is comma.
data division.
working-storage section.
01 s pic 9(5)v99 value 45296,5.
01 t pic 9(5)v99 value 1,25.
01 x pic x(5) value "abcde".
01 k pic 9 value 0.
procedure division.
declaratives.
d section.
    use after exception condition ec-bound-ref-mod.
d1.
    display "  EC-BOUND-REF-MOD".
end declaratives.
main section.
m1.
    display s " " t
    compute s = s + 0,5 display s
    >>turn ec-bound-ref-mod checking on
    display "[" x(1:k) "] (not expected: the run unit ends in the declarative)"
    stop run.
