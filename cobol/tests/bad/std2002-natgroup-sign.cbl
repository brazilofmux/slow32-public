identification division.
program-id. ngsign.
*> A signed numeric item in a national group is SIGN SEPARATE (2023 13.18.29.3 rule 3).
data division.
working-storage section.
01  g group-usage national.
    05 s pic s99.
procedure division.

    stop run.
