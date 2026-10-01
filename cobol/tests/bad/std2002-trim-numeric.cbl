identification division.
program-id. trimnum.
*> TRIM takes an alphabetic, alphanumeric or national argument (rule 1).
data division.
working-storage section.
01 k pic 9(4) value 12.
procedure division.
    display function trim(k)
    stop run.
