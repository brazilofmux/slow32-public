identification division.
program-id. ngusage.
*> A national group has no USAGE clause of its own (2023 13.18.29.3 rule 3).
data division.
working-storage section.
01  g group-usage national usage national.
    05 a pic n(2).
procedure division.
    stop run.
