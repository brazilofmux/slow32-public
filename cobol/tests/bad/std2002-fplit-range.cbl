identification division.
program-id. p.
*> The exponent's range is the implementor's (2023 8.3.3.3.3 rule 3): here
*> the one that keeps the value within 31 digits.
data division.
working-storage section.
01  a usage float-long value 1.0E+40.
procedure division.
    stop run.
end program p.
