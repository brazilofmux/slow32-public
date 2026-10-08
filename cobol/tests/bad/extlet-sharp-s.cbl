identification division.
program-id. p-extlet-sharp-s.
*> ß is no case pair of SS: GRÖSSE is not Größe (14.1; the simple pairs only).
data division.
working-storage section.
01 Größe pic 9.
procedure division.
    move 1 to GRÖSSE.
    stop run.
