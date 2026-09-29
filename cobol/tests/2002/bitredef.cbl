identification division.
program-id. bitredef.
*> REDEFINES with bit items (2023 13.18.44.4 rule 1: storage association
*> starts at the first bit of the redefined item; cobol ISSUES-85).  A
*> bit view of a byte item reads its bits; a byte view of a bit item
*> that starts a byte reads its bytes; a bit item redefining a bit item
*> inside a byte starts at that item's bit; a bit array redefines a
*> two-byte item, sixteen flags.
*> No oracle (docs/boolean.md).
data division.
working-storage section.
01  rec.
    05 c     pic x value "A".
    05 cb    redefines c pic 1(8) usage bit.
    05 f1    pic 1(3) usage bit value b"101".
    05 f2    pic 1(5) usage bit value b"11001".
    05 f2v   redefines f2 pic 1(5) usage bit.
    05 g     pic 1(8) usage bit value b"01100001".
    05 gx    redefines g pic x.
01  w        pic x(2) value x"8001".
01  wf redefines w.
    05 fl    pic 1 usage bit occurs 16.
procedure division.
main.
    display "bits of A: " cb
    move b"01100010" to cb
    display "A after its bits become 01100010: " c
    display "f2 through its redefinition: " f2v
    move b"00111" to f2v
    display "f1 and f2 after f2v receives 00111: " f1 " " f2
    display "g as a character: " gx
    display "flags 1, 2, 16: " fl(1) fl(2) fl(16)
    stop run.
