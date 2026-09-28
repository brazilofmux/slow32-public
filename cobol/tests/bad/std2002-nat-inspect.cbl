identification division.
program-id. natinsp.
*> INSPECT of a national item takes national operands only (2023
*> 14.9.22.3 rule 4): "a" is an alphanumeric literal; N"a" is its form.
data division.
working-storage section.
01  n pic n(4).
01  c pic 99.
procedure division.
    inspect n tallying c for all "a"
    stop run.
