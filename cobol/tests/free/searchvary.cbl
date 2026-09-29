*> SEARCH (X3.23-1985 SEARCH GR; 2023 14.9.37.4 GR 3b2 and 4): VARYING
*> an integer item increments it by one with each step of the index,
*> from whatever it held; an index already past the table ends the
*> search at once, AT END.  docs/conformance/search.md
*> No oracle: GnuCOBOL sets the VARYING item from the index, and searches
*> from the first occurrence when the index is past the table.
identification division.
program-id. searchvary.
data division.
working-storage section.
01 t.
   05 e occurs 6 indexed by i.
      10 c pic x.
01 k pic 99.
procedure division.
    move "abcbca" to t
    set i to 1 move 10 to k
    search e varying k at end display "v1 end" when c (i) = "c" display "v1 " k end-search
    set i to 2 move 10 to k
    search e varying k at end display "v2 end" when c (i) = "a" display "v2 " k end-search
    set i to 7
    search e at end display "v3 end" when c (i) = "a" display "v3 found" end-search
    stop run.
