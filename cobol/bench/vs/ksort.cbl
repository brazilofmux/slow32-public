*> ksort -- SORT with an INPUT and an OUTPUT PROCEDURE: 400,000 records
*> from a linear congruential generator, on an alphanumeric key ascending
*> and a COMP-3 key descending; a checksum of the output order.
identification division.
program-id. ksort.
environment division.
input-output section.
file-control.
    select w assign to "ksort.tmp".
data division.
file section.
sd  w.
01  wrec.
    05  w-name   pic x(8).
    05  w-amt    pic s9(7)v99 comp-3.
    05  w-seq    pic 9(9) comp.
    05  w-pad    pic x(20).
working-storage section.
01  n        pic 9(9) comp value 400000.
01  i        pic 9(9) comp.
01  seed     pic 9(18) comp value 12345.
01  eof      pic x value "n".
01  np      pic 9(9) comp value 0.
01  ck       pic 9(18) comp value 0.
procedure division.
    sort w on ascending key w-name on descending key w-amt
        input procedure gen output procedure chk
    display "ksort " np " " ck
    stop run.
gen.
    perform varying i from 1 by 1 until i > n
        compute seed = function mod(seed * 1103515245 + 12345, 2147483648)
        move function mod(seed, 26) to np
        move "KEYPAD" to w-name(2:6)
        move function char(66 + np) to w-name(1:1)
        move function mod(seed / 26, 10) to np
        move np to w-name(8:1)
        compute w-amt = function mod(seed, 9999991) / 100
        move i to w-seq
        release wrec
    end-perform.
chk.
    move 0 to np
    perform until eof = "y"
        return w at end move "y" to eof
            not at end
                add 1 to np
                compute ck = function mod(ck * 31 + w-seq, 1000000007)
        end-return
    end-perform.
