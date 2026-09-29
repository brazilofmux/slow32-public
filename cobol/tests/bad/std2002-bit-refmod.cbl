identification division.
program-id. bitrm.
*> Reference modification of a USAGE BIT item at a computed position comes later.
data division.
working-storage section.
01  b pic 1(8) usage bit.
01  k pic 9 value 2.
procedure division.
    display b(k:3)
    stop run.
