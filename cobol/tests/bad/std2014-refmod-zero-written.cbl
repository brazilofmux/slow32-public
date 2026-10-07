identification division.
program-id. p-std2014-refmod-zero-written.
*> A written length of zero in a reference modification without
*> >>REF-MOD-ZERO-LENGTH ON (2023 8.4.3.3.3 rule 5c; 7.3.23).
data division.
working-storage section.
01 x pic x(5) value "abcde".
01 n pic 9 value 0.

procedure division.
    display x(2:0)
    stop run.
