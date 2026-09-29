identification division.
program-id. mv.
data division.
working-storage section.
01 ne pic nbn value n"1 2".
01 n pic 9(3).
procedure division.
    move ne to n
    stop run.
