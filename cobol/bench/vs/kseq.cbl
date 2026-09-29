*> kseq -- sequential file I/O: 1,000,000 records of 100 bytes written,
*> then read back and a field summed.
identification division.
program-id. kseq.
environment division.
input-output section.
file-control.
    select f assign to "kseq.dat" organization sequential.
data division.
file section.
fd  f.
01  frec.
    05  f-key    pic 9(9).
    05  f-amt    pic s9(7)v99 comp-3.
    05  f-text   pic x(86).
working-storage section.
01  n        pic 9(9) comp value 1000000.
01  i        pic 9(9) comp.
01  eof      pic x value "n".
01  tot      pic s9(15)v99 comp-3 value 0.
01  cnt      pic 9(9) comp value 0.
procedure division.
    open output f
    perform varying i from 1 by 1 until i > n
        move i to f-key
        compute f-amt = i / 7
        move "SEQUENTIAL RECORD TEXT" to f-text
        write frec
    end-perform
    close f
    open input f
    perform until eof = "y"
        read f at end move "y" to eof
            not at end add f-amt to tot add 1 to cnt
        end-read
    end-perform
    close f
    display "kseq " cnt " " tot
    stop run.
