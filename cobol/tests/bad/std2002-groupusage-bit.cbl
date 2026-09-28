identification division.
program-id. bitgx.
*> Every elementary item in a bit group is USAGE BIT (2023 13.18.29.3 rule 2).
data division.
working-storage section.
01  g group-usage bit.
    05 a pic 1(4).
    05 b pic x usage display.
procedure division.

    stop run.
