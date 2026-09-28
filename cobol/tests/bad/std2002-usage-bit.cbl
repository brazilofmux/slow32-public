identification division.
program-id. bitocc.
*> OCCURS on a USAGE BIT item comes later; refused by name.
data division.
working-storage section.
01  t.
    05 b pic 1 usage bit occurs 8.
procedure division.

    stop run.
