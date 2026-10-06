identification division.
program-id. gsync.
*> SYNCHRONIZED on a group is COBOL 2023 (13.18.55.3 rule 1, E.3.2 item
*> 6); 2002 and 2014 take it on an elementary item only (2002 13.16.53
*> rule 1).  -std=2002 accepted it.
data division.
working-storage section.
01 g sync.
   05 a pic x.
procedure division.
    goback.
