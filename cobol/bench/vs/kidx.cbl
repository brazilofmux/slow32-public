*> kidx -- indexed file I/O: 100,000 records written in scrambled key
*> order, 100,000 random READs by key, then a full sequential pass.
identification division.
program-id. kidx.
environment division.
input-output section.
file-control.
    select f assign to "kidx.dat" organization indexed
        access dynamic record key f-key file status fs.
data division.
file section.
fd  f.
01  frec.
    05  f-key    pic 9(9).
    05  f-amt    pic s9(7)v99 comp-3.
    05  f-text   pic x(50).
working-storage section.
01  n        pic 9(9) comp value 100000.
01  i        pic 9(9) comp.
01  k        pic 9(9) comp.
01  fs       pic xx.
01  eof      pic x value "n".
01  tot      pic s9(15)v99 comp-3 value 0.
01  hits     pic 9(9) comp value 0.
01  cnt      pic 9(9) comp value 0.
procedure division.
    open output f
    perform varying i from 1 by 1 until i > n
        compute k = function mod(i * 7919, n) + 1
        move k to f-key
        compute f-amt = k / 3
        move "INDEXED RECORD" to f-text
        write frec invalid key display "dup " k end-write
    end-perform
    close f
    open i-o f
    perform varying i from 1 by 1 until i > n
        compute k = function mod(i * 104729, n) + 1
        move k to f-key
        read f invalid key continue
            not invalid key add 1 to hits add f-amt to tot
        end-read
    end-perform
    move 0 to f-key
    start f key > f-key invalid key display "start " fs end-start
    perform until eof = "y"
        read f next at end move "y" to eof
            not at end add 1 to cnt
        end-read
    end-perform
    close f
    display "kidx " hits " " cnt " " tot
    stop run.
