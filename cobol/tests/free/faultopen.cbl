identification division.
program-id. faultopen.
*> OPEN's permanent-error statuses under injected failures (S32_FAULT in
*> faultopen.env: the host's MMIO service fails the Nth OPEN request with
*> that errno; tools/emulator/mmio_ring.c).  X3.23-1985 VII-3: 35 a
*> non-optional file not present; 37 a file that will not support the
*> open mode.  A file that is there but cannot be opened was 35, and an
*> OPTIONAL one taken for absent was created anew, losing its records.
*> No oracle: the faults are this platform's.
environment division.
input-output section.
file-control.
    select optional f1 assign to "tmp/faultf1.dat"
        organization line sequential file status st1.
    select f2 assign to "tmp/faultnone.dat"
        organization line sequential file status st2.
data division.
file section.
fd f1.
01 r1 pic x(10).
fd f2.
01 r2 pic x(10).
working-storage section.
01 st1 pic xx.
01 st2 pic xx.
01 eof pic x value "N".
procedure division.
*> open 1 fails EACCES: OUTPUT on a file that will not be written, 37
    open output f1
    display "output, not permitted: " st1
*> open 2 succeeds: one record
    open output f1
    write r1 from "REC1"
    close f1
    display "output: " st1
*> open 3 fails EACCES: INPUT on a file that is there, 37 (it was 35)
    open input f1
    display "input, not permitted: " st1
*> open 4, EXTEND's look at the file, fails EACCES: the file is there,
*> so EXTEND goes on (open 5) and appends -- it was taken for absent
*> and created anew, REC1 lost
    open extend f1
    display "extend, unreadable: " st1
    write r1 from "REC2"
    close f1
    open input f1
    perform until eof = "Y"
        read f1
            at end move "Y" to eof
            not at end display "  read " r1
        end-read
        if st1 not = "00" and st1 not = "10"
            display "  read status " st1
            move "Y" to eof
        end-if
    end-perform
    close f1
*> a non-optional file not present: 35
    open input f2
    display "input, absent: " st2
    stop run.
