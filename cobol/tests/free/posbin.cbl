*> A positioned DISPLAY of a binary or packed item shows what a plain
*> DISPLAY shows -- sign, digits, point -- in a field that wide.  It was
*> cut to the item's bytes of storage: 1234 in PIC 9(4) COMP showed "12",
*> a BINARY-LONG "0000".
*> No oracle: positioned I/O needs a real tty.
identification division.
program-id. posbin.
data division.
working-storage section.
01  a  pic 9(4) comp value 1234.
01  l  binary-long value 123456.
01  bc binary-char value 42.
01  p3 pic s9(3)v99 comp-3 value -12.5.
procedure division.
    display a line 1 position 1
    display l line 2 position 1
    display bc line 3 position 1
    display p3 line 4 position 1
    display '|' line 5 position 1 a '|' l '|' bc '|' p3 '|'
    stop run.
