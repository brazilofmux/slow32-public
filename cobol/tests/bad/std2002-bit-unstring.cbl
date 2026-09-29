identification division.
program-id. bitunstr.
*> An UNSTRING receiver is of usage display or national (2023 14.9.48.3 rule 4).
data division.
working-storage section.
01  s pic x(8) value "10,01".
01  b pic 1(8) usage bit.
procedure division.
    unstring s delimited by "," into b
    stop run.
