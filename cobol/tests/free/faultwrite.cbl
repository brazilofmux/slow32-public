identification division.
program-id. faultwrite.
*> WRITE and CLOSE under an injected full device (S32_FAULT in
*> faultwrite.env fails the first two file writes with ENOSPC;
*> tools/emulator/mmio_ring.c).  X3.23-1985 VII-3: 34 a write beyond the
*> file's externally defined boundaries -- here the device full; 30 a
*> permanent error with no more to say.  The guest's stdio buffers 4K:
*> file a's three short records reach the device only at CLOSE, where a
*> failure was reported as 00 (libcob ignored fclose, and fclose its
*> flush); file b's long records reach it during a WRITE, which takes 34
*> and runs b's declarative.  The four records buffered before it were
*> in the failed flush too: buffered output reports a device error at
*> the first statement that sees it.  No oracle: the faults are this
*> platform's.
environment division.
input-output section.
file-control.
    select fa assign to "tmp/faulta.dat"
        organization sequential file status sta.
    select fb assign to "tmp/faultb.dat"
        organization sequential file status stb.
data division.
file section.
fd fa.
01 ra pic x(10).
fd fb.
01 rb pic x(1000).
working-storage section.
01 sta pic xx.
01 stb pic xx.
01 k pic 99.
procedure division.
declaratives.
b-error section.
    use after error procedure on fb.
b1.
    display "  declarative for fb: " stb.
end declaratives.
main section.
m1.
    open output fa
    write ra from "A1"
    write ra from "A2"
    write ra from "A3"
    display "fa writes: " sta
    close fa
    display "fa close, device full: " sta
    open output fb
    perform varying k from 1 by 1 until k > 6
        move all "B" to rb
        write rb
        display "fb write " k ": " stb
    end-perform
    close fb
    display "fb close: " stb
    stop run.
