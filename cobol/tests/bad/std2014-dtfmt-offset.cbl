identification division.
program-id. p-std2014-dtfmt-offset.
*> The offset argument goes with a UTC or offset time format (2023 15.41.3 rule 5).
data division.
working-storage section.
01 n pic 9(10) value 155508.
01 s pic 9(5)v99 value 1.
01 f pic x(8) value "YYYYMMDD".
01 nn pic n(8).
01 r pic x(20).
procedure division.
    move function formatted-time("hh:mm:ss", s, 60) to r
    stop run.
