*> ADDRESS OF as a CALL argument BY REFERENCE and BY CONTENT (2023
*> 8.4.3.11 GR 1: it creates a unique data item holding the address): the
*> callee gets a pointer, sets its LINKAGE record over the caller's item
*> and changes it -- by either mode, since what is copied is the pointer.
*> docs/conformance/usage.md
identification division.
program-id. addressofarg.
data division.
working-storage section.
01 w pic x(5) value "hello".
procedure division.
    call "peek" using by reference address of w
    call "peek" using by content address of w
    display "w now [" w "]"
    stop run.
end program addressofarg.
identification division.
program-id. peek.
data division.
linkage section.
01 lp usage pointer.
01 lk pic x(5).
procedure division using lp.
    set address of lk to lp
    display "callee sees [" lk "]"
    move "HELLO" to lk
    goback.
end program peek.
