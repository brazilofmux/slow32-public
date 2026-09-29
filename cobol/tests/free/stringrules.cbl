*> STRING's and UNSTRING's general rules at their edges (X3.23-1985
*> VI-131.., VI-136..; 2023 14.9.43.4, 14.9.48.4): the POINTER out of
*> range, a receiver filled only as far as the transfer, multi-character
*> and figurative delimiters, a numeric sender, DELIMITED BY ALL, the
*> first of several delimiters at a position, numeric and JUSTIFIED
*> receivers, DELIMITER IN, COUNT IN, TALLYING IN and the overflow
*> conditions.  docs/conformance/string.md
identification division.
program-id. stringrules.
data division.
working-storage section.
01 d   pic x(8).
01 p   pic 99.
01 n3  pic 9(3) value 7.
01 src pic x(20).
01 r1  pic x(4).
01 r2  pic x(4).
01 r3  pic x(4).
01 rj  pic x(4) justified right.
01 rn  pic 9(3).
01 rv  pic 9v9.
01 d1  pic x(2).
01 d2  pic x(2).
01 c1  pic 99.
01 c2  pic 99.
01 t   pic 99.
01 f   pic x(3).
procedure division.
    move all "." to d move 0 to p move "no" to f
    string "abc" delimited by size into d with pointer p
        on overflow move "ovf" to f end-string
    display "s1 " d " " p " " f
    move all "." to d move 3 to p move "no" to f
    string "abc" "defgh" delimited by size into d with pointer p
        on overflow move "ovf" to f
        not on overflow move "ok" to f end-string
    display "s2 " d " " p " " f
    move all "." to d
    string "xxabyy" delimited by "ab" " q r" delimited by space
        n3 delimited by size into d
    display "s3 " d
    move all "." to d move 1 to p move "no" to f
    string "abc" delimited by "z" into d with pointer p
        not on overflow move "ok" to f end-string
    display "s4 " d " " p " " f
    move "a  b c" to src
    move all "." to r1 r2 r3 move 0 to t
    unstring src delimited by all space
        into r1 count in c1 r2 count in c2 r3 tallying in t
    display "u1 " r1 "|" r2 "|" r3 " " c1 " " c2 " " t
    move "a,,b" to src move all "." to r1 r2 r3
    unstring src delimited by "," or ",," into r1 delimiter in d1 r2 delimiter in d2 r3
    display "u2 " r1 "|" r2 "|" r3 " " d1 "|" d2
    move "a,,b" to src move all "." to r1 r2 r3
    unstring src delimited by ",," or "," into r1 delimiter in d1 r2 delimiter in d2 r3
    display "u3 " r1 "|" r2 "|" r3 " " d1 "|" d2
    move "12,3,ab" to src
    unstring src delimited by "," or space into rn rv rj
    display "u4 " rn " " rv " " rj
    move "a;b;c;d" to src move 0 to t move 1 to p move "no" to f
    unstring src delimited by ";" into r1 r2 with pointer p tallying in t
        on overflow move "ovf" to f end-unstring
    display "u5 " r1 "|" r2 " " p " " t " " f
    move 0 to p move "no" to f move all "." to r1
    unstring src delimited by ";" into r1 with pointer p
        on overflow move "ovf" to f end-unstring
    display "u6 " r1 " " p " " f
    move "ab--cd" to src move all "." to r1 r2
    unstring src delimited by all "-" into r1 delimiter in d1 r2
    display "u7 " r1 "|" r2 " " d1
    move "a,b" to src move 0 to t move "no" to f
    unstring src delimited by "," into r1 r2 r3 tallying in t
        not on overflow move "ok" to f end-unstring
    display "u8 " r1 "|" r2 "|" r3 " " t " " f
    stop run.
