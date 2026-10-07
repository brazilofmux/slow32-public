identification division.
program-id. pr.
*> The header's RAISING names level-3 EC-USER exception-names (2023 14.2.2 rule 7).
data division.
working-storage section.
01 a pic 9.
procedure division raising ec-range-search-no-match.
    display a
    goback.
