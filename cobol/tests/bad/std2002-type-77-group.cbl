identification division.
program-id. td77.
*> A level 77 item takes an elementary type (2023 13.18.57.3 rule 7).
data division.
working-storage section.
01  g-t typedef.
    05 a pic x.
77  v type to g-t.
procedure division.

    stop run.
