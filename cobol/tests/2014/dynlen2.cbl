identification division.
program-id. dynlen2.
*> Dynamic-length items, the rest (2023 8.5.1.10, 14.9.39 format 16): a
*> VALUE that is a figurative constant (one character), SET SIZE OF with
*> EC-STORAGE-NOT-AVAIL for a negative size and for one past the LIMIT (the
*> length 0, or the maximum), an item passed BY REFERENCE and the group
*> holding one likewise (the slot is the caller's), a called program's
*> items that CANCEL puts back to their VALUEs, items in a table.
*> No oracle: GnuCOBOL 4 has no DYNAMIC LENGTH.  No gcobol either.
data division.
working-storage section.
01 f pic x dynamic length value space.
01 q pic x dynamic length limit 8 value "abc".
01 g.
   05 k pic 9(2) value 7.
   05 d pic x dynamic length limit 12.
   05 e pic x(2) value "ee".
01 i pic s9(4).
procedure division.
declaratives.
d1 section. use after exception condition ec-storage-not-avail.
p1. display "  nonfatal: " function trim(function exception-status) " len=" function length(q).
end declaratives.
main section.
m1.
    display "value space: f=[" f "] len=" function length(f).
>>TURN EC-STORAGE-NOT-AVAIL CHECKING ON
    set size of q to 5.
    display "size 5: q=[" q "] len=" function length(q).
    move -2 to i.
    set size of q to i.
    display "size -2: q=[" q "] len=" function length(q).
    move 20 to i.
    set size of q to i.
    display "size 20 (limit 8): q=[" q "] len=" function length(q).
    move "hello" to d.
    call "dl2sub" using d g.
    display "after sub: d=[" d "] len=" function length(d) " k=" k " e=" e.
    call "dl2own". call "dl2own".
    cancel "dl2own".
    call "dl2own".
    display "done".
    stop run.
identification division.
program-id. dl2sub.
data division.
linkage section.
01 ld pic x dynamic length limit 12.
01 lg.
   05 lk pic 9(2).
   05 ldd pic x dynamic length limit 12.
   05 le pic x(2).
procedure division using ld lg.
    display "  sub: ld=[" ld "] len=" function length(ld) " ldd=[" ldd "] lk=" lk " le=" le.
    move "changed by sub" to ld.
    move 9 to lk.
    display "  sub set: ld=[" ld "] len=" function length(ld).
    goback.
end program dl2sub.
identification division.
program-id. dl2own.
data division.
working-storage section.
01 og.
   05 calls pic 9 value 0.
   05 ot pic x dynamic length occurs 2 value "init".
01 ow pic x(20).
procedure division.
    add 1 to calls.
    display "  own: call " calls " ot(1)=[" ot(1) "] ot(2)=[" ot(2) "]".
    move "first" to ot(1).
    move spaces to ow. string ot(2) "+" delimited by size into ow.
    move function trim(ow) to ot(2).
    goback.
end program dl2own.
end program dynlen2.
