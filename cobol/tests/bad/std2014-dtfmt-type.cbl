identification division.
program-id. p-std2014-dtfmt-type.
*> The data is of the format's type (2023 15.92.3 rule 2).
data division.
working-storage section.
01 n pic 9(10) value 155508.
01 s pic 9(5)v99 value 1.
01 f pic x(8) value "YYYYMMDD".
01 nn pic n(8).
01 r pic x(20).
procedure division.
    move function test-formatted-datetime("YYYYMMDD", nn) to n
    stop run.
