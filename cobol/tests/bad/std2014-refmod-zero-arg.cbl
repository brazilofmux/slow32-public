identification division.
program-id. p-std2014-refmod-zero-arg.
*> >>REF-MOD-ZERO-LENGTH takes ON or OFF (2023 7.3.23.2).
data division.
working-storage section.
01 x pic x(5) value "abcde".
01 n pic 9 value 0.

procedure division.
    >>ref-mod-zero-length maybe
    display x(2:n)
    stop run.
