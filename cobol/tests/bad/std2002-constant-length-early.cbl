identification division.
program-id. std2002constantlengthearly.
*> A LENGTH OF constant has its value once the DATA DIVISION is laid out;
*> used inside it, it is refused (not implemented).
data division.
working-storage section.
01 r pic x(10).
01 n constant as length of r.
01 t pic 99 value n.
procedure division.
    stop run.
