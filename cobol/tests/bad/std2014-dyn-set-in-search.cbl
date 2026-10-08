identification division.
program-id. p-std2014-dyn-set-in-search.
*> SET of the capacity inside a SEARCH of the table (2023 14.9.39.4 rule 31, EC-FLOW-SEARCH).
data division.
working-storage section.
01 g. 05 t pic 9 occurs dynamic capacity in c indexed by ix.
procedure division.
    set ix to 1
    search t at end display "no" when t(ix) = 5 set c up by 1 end-search
    goback.
