identification division.
program-id. fnrmpast.
*> A run-time-length function result reference-modified to its end
*> (start:) with a start past the result's end raises EC-BOUND-REF-MOD
*> when checked (8.4.3.3.4 rule 5; cobol ISSUES-94 E3) -- a fatal
*> condition; a start inside it is fine.
*> No oracle: GnuCOBOL 4 does not implement exception declaratives.
procedure division.
declaratives.
ub section.
    use after exception condition ec-bound-ref-mod.
u1.
    display "  EC-BOUND-REF-MOD".
end declaratives.
main section.
m1.
>>TURN EC-BOUND-REF-MOD CHECKING ON
    display "(2:) [" function display-of(function national-of("ab"))(2:) "]"
    display "(5:) [" function display-of(function national-of("ab"))(5:) "] (not expected)"
    stop run.
