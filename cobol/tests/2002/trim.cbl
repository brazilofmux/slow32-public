*> FUNCTION TRIM (2014; 2023 15.96), taken under -std=2002 as BP-E27:
*> both ends, LEADING, TRAILING; an all-space argument giving a result of
*> length zero (returned value rule 4); the result as a MOVE's sender, a
*> DISPLAY operand, a STRING source and an argument of LENGTH and of
*> UPPER-CASE; a literal argument.  Six X-COBOL programs use it.
identification division.
program-id. trim.
data division.
working-storage section.
01 s       pic x(12) value "  two words ".
01 blank-s pic x(6)  value spaces.
01 w       pic x(16).
01 out     pic x(40).
01 p       pic 99.
procedure division.
    display "[" function trim(s) "]"
    display "[" function trim(s leading) "]"
    display "[" function trim(s trailing) "]"
    display "[" function trim(blank-s) "] " function length(function trim(blank-s))
    display function length(function trim(s)) " " function length(function trim(s trailing))
    move function trim(s) to w
    display "[" w "]"
    move 1 to p
    string function trim(s) delimited by size "|" delimited by size
           function trim("  lit  ") delimited by size into out with pointer p
    display "[" out(1:p - 1) "]"
    display function upper-case(function trim(s))
    if function trim(s) = "two words" display "compares equal" end-if
    stop run.
