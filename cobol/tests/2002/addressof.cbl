*> ADDRESS OF (2002 8.4.2.11; 2023 8.4.3.11), BASED entries (2002 13.16.5,
*> 8.6.4) and SET's data-pointer formats (14.9.39 formats 7 and 10): a
*> pointer set to an item's address, a based record laid over it, the
*> pointer moved UP and DOWN, compared, passed to a program that sets a
*> LINKAGE record's address from it, and set back to NULL.
*> docs/conformance/usage.md
identification division.
program-id. addressof.
data division.
working-storage section.
01 w        pic x(8) value "abcdefgh".
01 tbl.
   05 te    pic x(2) occurs 4.
01 p        usage pointer.
01 q        usage pointer.
01 b        pic x(8) based.
01 bt       based.
   05 bc    pic x occurs 8.
procedure division.
    move "11223344" to tbl
    set p to address of w
    set address of b to p
    display "[" b "]"
    move "XY" to b(1:2)
    display "[" w "]"
    set address of bt to address of w
    display bc(3) bc(8)
    set p up by 2
    set address of b to p
    display "[" b(1:3) "]"
    set p down by 1
    set address of b to p
    display "[" b(1:3) "]"
    if address of b = p display "same" else display "differ" end-if
    if address of b not = address of w display "moved" end-if
    set q to address of te(3)
    set address of b to q
    display "[" b(1:2) "]"
    set p to address of w
    call "showit" using p
    set address of b to null
    if address of b = null display "null again" end-if
    if p not = null display "p set" end-if
    stop run.
end program addressof.

identification division.
program-id. showit.
data division.
linkage section.
01 lp       usage pointer.
01 lk       pic x(8).
procedure division using lp.
    set address of lk to lp
    display "via linkage [" lk "]"
    goback.
end program showit.
