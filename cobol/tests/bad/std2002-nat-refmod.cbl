identification division.
program-id. natrm.
*> A national item's reference modification counts character positions
*> (2023 8.4.2.4): n(4:2) runs past the end of a PIC N(4), whose eight
*> bytes would hold it.
data division.
working-storage section.
01  n pic n(4) value n"abcd".
procedure division.
    display n(4:2)
    stop run.
