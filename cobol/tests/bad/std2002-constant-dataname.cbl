identification division.
program-id. std2002constantdataname.
*> A constant-name is not also a data item's name.
data division.
working-storage section.
01 k constant as 4.
01 k pic x.
procedure division.
    stop run.
