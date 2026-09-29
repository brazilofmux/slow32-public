*> EC-RANGE-SEARCH-INDEX (2023 14.9.37.4 GR 4): a serial SEARCH whose
*> index is outside the table at the start is unsuccessful and the
*> condition exists; the declarative runs, then AT END.  A SEARCH whose
*> index is in range is untouched.  docs/conformance/search.md
*> No oracle (the exception machinery is GnuCOBOL's own).
identification division.
program-id. ecsearchidx.
data division.
working-storage section.
01 t.
   05 e occurs 3 indexed by i.
      10 c pic x.
procedure division.
declaratives.
dx section.
    use after exception condition ec-range-search-index.
d1.
    display "declarative: " function exception-status.
end declaratives.
main section.
m1.
>>TURN EC-RANGE-SEARCH-INDEX CHECKING ON
    move "abc" to t
    set i to 4
    search e at end display "at end" when c (i) = "a" display "found" end-search
    set i to 2
    search e at end display "at end 2" when c (i) = "c" display "found c" end-search
    stop run.
