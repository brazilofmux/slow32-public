identification division.
function-id. wsret.
*> The RETURNING item belongs in the LINKAGE SECTION (2023 14.2.2 rule 5).
data division.
working-storage section.
01  r pic 9.
procedure division returning r.
    goback.
end function wsret.
