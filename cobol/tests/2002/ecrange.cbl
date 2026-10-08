identification division.
program-id. ecrange.
*> EC-RANGE-INVALID (2023 14.7.8: a THROUGH range whose start is above
*> its end -- nonfatal, then the range is empty) and EC-RANGE-INSPECT-SIZE
*> at run time (14.9.22.4 rules 14 and 22: REPLACING operands of unequal
*> size, one of them a part of computed length -- fatal; cobol queue item
*> 4).  No oracle (ecraise).
data division.
working-storage section.
01 v pic 99 value 4.
01 lo pic 99 value 5.
01 hi pic 99 value 3.
01 w pic x(10) value "abcabcabca".
01 n pic 9 value 2.
01 m pic 9 value 3.
procedure division.
declaratives.
d1 section. use after exception condition ec-range-invalid.
p1. display "  nonfatal: " function trim(function exception-status).
d2 section. use after exception condition ec-range-inspect-size.
p2. display "  fatal: " function trim(function exception-status).
end declaratives.
main section.
m1.
>>TURN EC-RANGE-INVALID EC-RANGE-INSPECT-SIZE CHECKING ON
    evaluate v
        when lo thru hi display "in the reversed range?"
        when 1 thru 9 display "in 1 thru 9"
    end-evaluate.
    evaluate v
        when 4 thru 2 display "in 4 thru 2?"
        when other display "other"
    end-evaluate.
    evaluate v
        when 2 thru 6 display "in 2 thru 6: fine"
    end-evaluate.
    inspect w replacing all w(1:n) by "xy".
    display "equal sizes: " w.
    inspect w replacing all w(1:n) by w(4:m).
    display "not reached: " w.
    stop run.
