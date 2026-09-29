identification division.
program-id. fnrmzero.
*> A function result reference-modified at computed positions (cobol
*> ISSUES-94). E1: the start and length are evaluated before the
*> function, so functions inside them no longer overwrite its result --
*> (LENGTH(DISPLAY-OF(NATIONAL-OF("xy"))):) of "abcdefgh" is "bcdefgh".
*> E2: a computed length of 0 is out of range (8.4.3.3.4 rule 5) and,
*> checked, raises EC-BOUND-REF-MOD, a fatal condition -- it no longer
*> means "to the end".
*> No oracle: GnuCOBOL 4 does not implement exception declaratives.
data division.
working-storage section.
01  a  pic x(8) value "abcdefgh".
01  n  pic 9 value 0.
procedure division.
declaratives.
ub section.
    use after exception condition ec-bound-ref-mod.
u1.
    display "  EC-BOUND-REF-MOD".
end declaratives.
main section.
m1.
    display "E1: [" function display-of(function national-of("abcdefgh"))(function length(function display-of(function national-of("xy"))):) "]"
    display "E1: [" function upper-case(a)(function length(function display-of(function national-of("x"))) + function length(function display-of(function national-of("y"))) - 1 : 3) "]"
>>TURN EC-BOUND-REF-MOD CHECKING ON
    display "E2: [" function upper-case(a)(1:n) "] (not expected)"
    stop run.
