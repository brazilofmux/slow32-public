identification division.
program-id. p-std2014-dyn-set-not-cap.
*> SET UP BY on an integer item that is no index and no capacity (1985 SET; 2023 14.9.39.3).
data division.
working-storage section.
01 g. 05 t pic x occurs dynamic capacity in c.
01 n pic 9.
procedure division.
    set n up by 1
    goback.
