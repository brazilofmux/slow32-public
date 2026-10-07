identification division.
program-id. p-std2002-refmod-zero-directive.
*> >>REF-MOD-ZERO-LENGTH is COBOL 2023 (7.3.23), taken under -std=2014.
data division.
working-storage section.
01 x pic x(5) value "abcde".
01 n pic 9 value 0.

procedure division.
    >>ref-mod-zero-length on
    display x(2:n)
    stop run.
