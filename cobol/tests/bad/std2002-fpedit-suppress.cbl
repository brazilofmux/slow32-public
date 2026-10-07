identification division.
program-id. p.
*> The significand of a floating-point numeric-edited PICTURE takes no
*> zero suppression, no floating insertion (2023 13.18.40.3 rule 13b).
data division.
working-storage section.
01  e pic ZZ9.99E+99.
procedure division.
    stop run.
end program p.
