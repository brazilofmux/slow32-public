identification division.
program-id. p-std2014-dtfmt-kind.
*> FORMATTED-DATE takes a date format (2023 15.39.3 rule 2).
data division.
working-storage section.
01 n pic 9(10) value 155508.
01 s pic 9(5)v99 value 1.
01 f pic x(8) value "YYYYMMDD".
01 nn pic n(8).
01 r pic x(20).
procedure division.
    move function formatted-date("hhmmss", n) to r
    stop run.
