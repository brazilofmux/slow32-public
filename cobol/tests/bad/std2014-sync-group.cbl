identification division.
program-id. p-std2014-sync-group.
*> SYNCHRONIZED on a group is COBOL 2023 (13.18.55.3 rule 1; E.3.2 item 6).
data division.
working-storage section.
01 g sync.
   05 a pic x.
   05 b pic s9(4) comp.
procedure division.
    display g
    stop run.
