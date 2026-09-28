identification division.
program-id. inspnat.
*> INSPECT of an alphanumeric item takes no national operand (2023
*> 14.9.22.3 rule 4): the national literal would be compared as bytes.
data division.
working-storage section.
01  a pic x(4).
01  c pic 99.
procedure division.
    inspect a tallying c for all n"a"
    stop run.
