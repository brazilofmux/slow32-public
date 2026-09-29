identification division.
program-id. bitrm.
*> Reference modification of a bit array element comes later; refused by name.
data division.
working-storage section.
01  t.
    05 b pic 1(4) usage bit occurs 3.
procedure division.
    display b(2)(1:2)
    stop run.
