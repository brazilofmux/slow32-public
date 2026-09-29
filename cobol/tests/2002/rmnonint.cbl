identification division.
program-id. rmnonint.
*> A computed reference-modification position that is not an integer is
*> out of range (8.4.3.3.4 rule 5; cobol ISSUES-94 E17): checked, it
*> raises EC-BOUND-REF-MOD, a fatal condition, instead of being
*> truncated. An integer computed position is fine.
*> No oracle: GnuCOBOL 4 does not implement exception declaratives.
data division.
working-storage section.
01  a  pic x(8) value "abcdefgh".
01  k  pic 9v9 value 2.0.
01  h  pic 9v9 value 2.5.
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
    display "k = 2.0: [" a(k:1) "]"
    display "h = 2.5: [" a(h:1) "] (not expected)"
    stop run.
