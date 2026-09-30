*> 31 digits, phase 3 (COBOL 2002; docs/wide.md): the exact intrinsic
*> functions with arguments past 18 digits (MAX, MIN, ORD-MAX, SUM, RANGE,
*> MIDRANGE, MOD, REM, INTEGER, INTEGER-PART, ABS, SIGN, FRACTION-PART),
*> NUMVAL, NUMVAL-C and NUMVAL-F of long strings, SORT on keys past 18
*> digits (a table and a file, negatives among them), the NUMERIC class
*> test, INITIALIZE, SET, SEARCH ALL and EVALUATE on such items.
identification division.
program-id. wide4.
environment division.
input-output section.
file-control.
    select sf assign to "tmp/w4.srt".
    select fo assign to "tmp/w4.out" organization line sequential.
data division.
file section.
sd sf.
01 sr.
   05 sk pic s9(25).
   05 sn pic x(3).
fd fo.
01 orec pic x(28).
working-storage section.
01 a  pic s9(31) value -1234567890123456789012345678901.
01 b  pic s9(20)v9(10) value 12345678901234567890.0123456789.
01 c  pic s9(31).
01 d  pic s9(20)v9(11).
01 ax pic x(31) value "1234567890123456789012345678901".
01 an redefines ax pic 9(31).
01 e  pic 9(5).
01 t.
   05 te occurs 4 ascending key tk indexed by ti.
      10 tk pic s9(25).
01 i  pic 9.
procedure division.
    move function max(a b) to d display "max " d
    move function min(a b) to c display "min " c
    move function ord-max(a b 7) to e display "ordmax " e
    move function sum(b b) to d display "sum " d
    move function range(a b) to c display "range " c
    move function midrange(b 1) to d display "midrange " d
    move function mod(a 97) to c display "mod " c
    move function rem(a 97) to c display "rem " c
    move function integer(b) to c display "integer " c
    move function integer-part(-12345678901234567890.5) to c display "intpart " c
    move function abs(a) to c display "abs " c
    move function sign(a) to c display "sign " c
    move function fraction-part(b) to d display "frac " d
    move function numval("-1234567890123456789012345.5") to d display "numval " d
    move function numval-c("$1,234,567,890,123,456,789.25") to d display "numvalc " d
    move function numval-f("1.25E+20") to d display "numvalf " d
    compute c = function abs(a) + 1 display "abs+1 " c
    move -3000000000000000000000001 to tk (1) move 2000000000000000000000002 to tk (2)
    move -1000000000000000000000003 to tk (3) move 5 to tk (4)
    sort te ascending tk
    perform varying i from 1 by 1 until i > 4 display "tk " tk (i) end-perform
    search all te at end display "not found" when tk (ti) = 5 set e to ti display "found " e end-search
    sort sf on descending key sk input procedure mk giving fo
    open input fo
    perform 3 times read fo display "file " orec end-perform
    close fo
    if an is numeric display "numeric" else display "not numeric" end-if
    move "12345x7890123456789012345678901" to ax
    if an is numeric display "numeric" else display "not numeric" end-if
    initialize a display "init " a
    move 1234567890123456789012345678901 to a
    set e to a display "set " e
    evaluate a when 1 thru 1234567890123456789012345678 display "low" when other display "other" end-evaluate
    stop run.
mk.
    move -9999999999999999999999999 to sk move "neg" to sn release sr
    move 8888888888888888888888888 to sk move "pos" to sn release sr
    move 0 to sk move "zer" to sn release sr.
