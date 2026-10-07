identification division.
program-id. p.
*> The exponent of a floating-point numeric-edited PICTURE is '+' and one
*> to four 9s (2023 13.18.40.3 rule 13b).
data division.
working-storage section.
01  e pic 9.99E99.
procedure division.
    stop run.
end program p.
