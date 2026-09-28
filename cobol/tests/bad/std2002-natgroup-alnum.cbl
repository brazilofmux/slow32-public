identification division.
program-id. ngalnum.
*> Every elementary item in a national group is national (2023 13.18.29.3 rule 3).
data division.
working-storage section.
01  g group-usage national.
    05 a pic n(2).
    05 b pic x(2).
procedure division.
    stop run.
