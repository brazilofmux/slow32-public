*> Bit items in screen I/O (docs/plans/standard-queue.md item 12): a
*> SCREEN SECTION field FROM a bit item's part and USING a bit array's
*> element (PICTURE 1, a boolean field of 0 and 1 characters), a
*> positioned DISPLAY of a part and of a whole bit item in its positions
*> (it showed the item's bytes), a positioned ACCEPT into an element.
*> The keys come from posbits.keys; the ANSI stream is the expected
*> output.  No oracle: GnuCOBOL has no USAGE BIT, and its screens need a
*> real tty.
identification division.
program-id. posbits.
data division.
working-storage section.
01  b           pic 1(8) usage bit value b"10110011".
01  tb.
    05  arr     pic 1(3) usage bit occurs 3 value b"101".
01  tb-r redefines tb pic 1(9) usage bit.
screen section.
01  scr.
    05  line 2 column 3 pic 1(4) from b(3:4).
    05  line 3 column 3 pic 1(3) using arr(2).
    05  line 4 column 3 pic 1(8) from b.
procedure division.
    display scr
    display b(3:4) at line 6 column 1
    accept arr(2) at line 7 column 1
    display tb-r at line 8 column 1
    stop run.
end program posbits.
