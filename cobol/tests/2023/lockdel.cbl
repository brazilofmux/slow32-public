identification division.
program-id. lockdel.
*> DELETE FILE of a physical file open through another connector of the run
*> unit: 62 (2023 Table 19 note, 9.1.13.9), the file left alone; OPEN ...
*> RETRY FOR a fraction of a second before 61.  No oracle: GnuCOBOL's sharing
*> checks are between processes.  No gcobol either.
environment division.
input-output section.
file-control.
    select f1 assign to "ld.dat" organization relative access random relative key rk1 file status s1
        sharing with all other.
    select f2 assign to "ld.dat" organization relative access random relative key rk2 file status s2
        sharing with all other.
data division.
file section.
fd f1.
01 r1 pic x(4).
fd f2.
01 r2 pic x(4).
working-storage section.
01 s1 pic xx.
01 s2 pic xx.
01 rk1 pic 9.
01 rk2 pic 9.
procedure division.
    open output f1. move 1 to rk1. move "one " to r1. write r1. close f1.
    open i-o f1.
    delete file f2.
    display "delete file while open elsewhere: " s2.
    open output retry for 0.01 seconds f2.
    display "open output with retry: " s2.
    open i-o retry 3 times f2.
    display "open i-o with retry: " s2.
    move 1 to rk2. read f2.
    display "the file is still there: " s2 " " r2.
    close f1. close f2.
    delete file f2.
    display "delete file closed: " s2.
    open input f1.
    display "open after delete: " s1.
    stop run.
