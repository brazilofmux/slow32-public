identification division.
program-id. p.
*> A floating-point literal whose significand is zero has a zero exponent
*> and no minus sign (2023 8.3.3.3.3 rule 4).
data division.
working-storage section.
01  a pic 9 value 0.0E-1.
procedure division.
    stop run.
end program p.
