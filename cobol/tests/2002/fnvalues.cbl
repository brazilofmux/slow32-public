identification division.
program-id. fnvalues.
*> The numeric functions' returned values at full width (2023 15.x): the
*> functions computed in double (SQRT, LOG, EXP, MEAN, ...) were held to
*> nine integer digits, so MEAN(2000000000 3000000000) was 999999999, and
*> FACTORIAL stopped the run past 19.  They now give the value as wide as
*> it is, to the 15 significant digits a double carries, and FACTORIAL is
*> exact to 33!; a statement that uses one computes on the wide stack.
*> Integer arguments may be arithmetic expressions (15.3 rule 6):
*> DATE-OF-INTEGER(INTEGER-OF-DATE(d) + 30), which was refused.
data division.
working-storage section.
01 p25  pic 99 value 25.
01 r    pic s9(9)v9(6).
01 big  pic 9(31).
01 x    pic 9(5) value 2.
01 d    pic 9(8) value 20240215.
01 d2   pic 9(8).
procedure division.
    compute big = function factorial(p25)
    display big
    compute r = function sqrt(x) * 2
    display r
    compute r rounded = function sqrt(x)
    display r
    add function exp(1) to r
    display r
    if function sqrt(x) > 1.414 display "gt" end-if
    move function log(10) to r
    display r
    compute r = function mean(2000000000 3000000000) / 1000
    display r
    compute r = function variance(100000 200000 300000)
    display r
    compute r = function median(7 1 12.5 3)
    display r
    compute r = function standard-deviation(2 4 4 4 5 5 7 9)
    display r
    compute d2 = function date-of-integer(function integer-of-date(d) + 30)
    display d2
    compute r = function day-of-integer(function integer-of-date(d) - 45)
    display r
    stop run.
end program fnvalues.
