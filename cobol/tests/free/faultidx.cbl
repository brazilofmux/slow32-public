identification division.
program-id. faultidx.
*> An OPTIONAL indexed file that is there but cannot be opened I-O
*> (S32_FAULT in faultidx.env fails that OPEN with EACCES;
*> tools/emulator/mmio_ring.c): 37 (X3.23-1985 VII-3), and the file and
*> its records untouched.  It was taken for absent and created anew,
*> empty.  No oracle: the fault is this platform's.
environment division.
input-output section.
file-control.
    select optional fx assign to "tmp/faultx.dat"
        organization indexed access dynamic
        record key xk file status stx.
data division.
file section.
fd fx.
01 xr.
   05 xk pic x(4).
   05 xd pic x(6).
working-storage section.
01 stx pic xx.
01 eof pic x value "N".
procedure division.
    open output fx
    move "K001" to xk  move "first"  to xd  write xr
    move "K002" to xk  move "second" to xd  write xr
    close fx
    display "built: " stx
    open i-o fx
    display "i-o, not permitted: " stx
    open input fx
    display "input: " stx
    perform until eof = "Y"
        read fx next record
            at end move "Y" to eof
            not at end display "  read " xr
        end-read
        if stx not = "00" and stx not = "10"
            display "  read status " stx
            move "Y" to eof
        end-if
    end-perform
    close fx
    stop run.
