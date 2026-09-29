*> EC-DATA-INCOMPATIBLE (2023 14.6.13.2 rule 2): a numeric sending item
*> whose content fails the NUMERIC class test, referenced in a COMPUTE expression while
*> the condition is checked; fatal, the run ends after the declarative.
*> Unchecked the same ADD goes ahead.  docs/conformance/arithmetic.md.
*> No oracle (the exception machinery is GnuCOBOL's own).
identification division.
program-id. ecincompat2.
data division.
working-storage section.
01 raw  pic x(3) value "1a3".
01 n    redefines raw pic 9(3).
01 m    pic 9(3) value 5.
procedure division.
declaratives.
dx section.
    use after exception condition ec-data-incompatible.
d1.
    display "declarative: " function exception-status.
end declaratives.
main section.
m1.
    add 1 to m
    display "unchecked: " m
>>TURN EC-DATA-INCOMPATIBLE CHECKING ON
    compute m = n * 2 + 1
    display "not reached"
    stop run.
