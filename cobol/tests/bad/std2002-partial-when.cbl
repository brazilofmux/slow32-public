identification division.
program-id. p-partial-when.
*> A partial expression as a WHEN object under -std=2002: 2014's (2023
*> 14.9.13).
data division.
working-storage section.
01 n pic 9 value 1.
procedure division.
    evaluate n
        when > 0 display "x"
    end-evaluate
    goback.
