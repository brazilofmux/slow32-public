identification division.
program-id. faultbyte.
*> A full device under one-byte records (S32_FAULT in faultbyte.env fails
*> the program's first and third file writes with ENOSPC; faultwrite has
*> the long records).  A one-byte WRITE is a store into the stream's buffer
*> (libcob.c, "The short entries of READ and WRITE") until the buffer
*> has no room; the record that finds it full goes through fwrite, which
*> empties the buffer -- and that WRITE, the 4,096th, is the one that
*> takes 34 and runs the declarative, exactly where it did when every
*> record went through fwrite.  The 4,095 before it were in the failed
*> write and are lost; what is written after it arrives.  Then records of
*> eight bytes, which fill the buffer exactly: the 512th is the one that
*> fills it, and the one refused -- not the one after.  (Until the C
*> library's fwrite was repaired -- runtime ISSUES-28 -- neither file
*> reported anything: a request that filled the buffer exactly was
*> counted as written.)  No oracle: the faults are this platform's.
environment division.
input-output section.
file-control.
    select f assign to "tmp/faultbyte.dat"
        organization sequential file status st.
    select g assign to "tmp/faultbyte8.dat"
        organization sequential file status st.
data division.
file section.
fd f.
01 r pic x.
fd g.
01 r8 pic x(8).
working-storage section.
01 st pic xx.
01 i pic 9(5).
01 n pic 9(5) value 0.
01 eof pic x value "n".
procedure division.
declaratives.
f-error section.
    use after error procedure on f g.
f1.
    display "  declarative: " st " at record " i.
end declaratives.
main section.
m1.
    open output f
    perform varying i from 1 by 1 until i > 5000
        move "x" to r
        write r
        if st not = "00" display "write " i ": " st end-if
    end-perform
    close f
    display "close: " st
    open input f
    perform until eof = "y"
        read f at end move "y" to eof not at end add 1 to n end-read
    end-perform
    close f
    display "records in the file: " n
    open output g
    perform varying i from 1 by 1 until i > 600
        move "eightbyt" to r8
        write r8
        if st not = "00" display "write " i ": " st end-if
    end-perform
    close g
    display "close: " st
    move 0 to n  move "n" to eof
    open input g
    perform until eof = "y"
        read g at end move "y" to eof not at end add 1 to n end-read
    end-perform
    close g
    display "records in the file: " n
    stop run.
