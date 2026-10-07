identification division.
program-id. p-std2014-dtfmt-mixed.
*> A basic date with an extended time is no combined format (2023 15.3.3.7).
data division.
working-storage section.
01 n pic 9(10) value 155508.
01 s pic 9(5)v99 value 1.
01 f pic x(8) value "YYYYMMDD".
01 nn pic n(8).
01 r pic x(20).
procedure division.
    move function formatted-datetime("YYYYMMDDThh:mm:ss", n, s) to r
    stop run.
