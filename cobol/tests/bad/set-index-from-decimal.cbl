identification division.
program-id. p.
data division.
working-storage section.
01 t1.
   05 a pic x(3) occurs 5 indexed by i1.
01 n    pic 99.
01 nd   pic 9v9.
01 ixd  usage index.
procedure division.
    set i1 to nd
    stop run.
