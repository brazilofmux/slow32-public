identification division.
program-id. bitrm.
*> Reference modification of a USAGE BIT item comes later; refused by name.
data division.
working-storage section.
01  b pic 1(8) usage bit.
procedure division.
    display b(2:3)
    stop run.
