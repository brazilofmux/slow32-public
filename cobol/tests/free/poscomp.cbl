*> A positioned ACCEPT of a binary or packed item: the field is as wide
*> as the item's PICTURE (9(4) COMP is four columns, not its two bytes
*> of storage; 9(5) COMP-3 five, not three), and it is edited as the
*> number it is.  The keys come from poscomp.keys.
*> No oracle: positioned ACCEPT needs a real tty.
identification division.
program-id. poscomp.
data division.
working-storage section.
01  a  pic 9(4) comp value 0.
01  b  pic 9(5) comp-3 value 0.
01  c  pic s9(3)v99 comp value 0.
procedure division.
    accept a line 1 position 1
    accept b line 2 position 1
    accept c line 3 position 1
    display a line 5 position 1
    display b line 6 position 1
    display c line 7 position 1
    stop run.
