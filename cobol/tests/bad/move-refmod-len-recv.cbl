identification division.
program-id. rl.
data division.
working-storage section.
01 s pic x(6) value "abcdef".
01 k pic 9 value 2.
01 y pic x(6).
procedure division.
    move s(1:k) to k y
    stop run.
