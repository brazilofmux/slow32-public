*> SEARCH's general rules at their edges (X3.23-1985 SEARCH GR; 2023
*> 14.9.37.4): a serial search starts at the index's current value;
*> VARYING another table's index steps that; the first
*> WHEN that holds wins; SEARCH ALL on two keys, a descending key, a
*> condition-name of one value, and an OCCURS DEPENDING ON table searched
*> only as far as its count.  The index past the table and VARYING an
*> integer item: searchvary.  docs/conformance/search.md
identification division.
program-id. searchrules.
data division.
working-storage section.
01 t.
   05 e occurs 6 indexed by i.
      10 c pic x.
01 u.
   05 f pic 9 occurs 6 indexed by j.
01 w.
   05 we occurs 5 ascending key wk1 wk2 indexed by wi.
      10 wk1 pic x.
      10 wk2 pic 9.
         88 wk2-three value 3.
      10 wv pic x(3).
01 d.
   05 dt occurs 5 descending key dk indexed by di.
      10 dk pic 99.
01 n pic 9 value 3.
01 o.
   05 oe occurs 1 to 5 depending on n ascending key ok indexed by oi.
      10 ok pic 9.
01 k pic 99.
01 r pic x(12).
procedure division.
    move "abcbca" to t
    move "123456" to u
    set i to 3
    search e at end display "s1 end" when c (i) = "b" set k to i display "s1 " k end-search
    set i to 6
    search e at end display "s2 end" when c (i) = "b" set k to i display "s2 " k end-search
    set i to 1 set j to 1
    search e varying j at end display "s4 end" when c (i) = "c" display "s4 " f (j) end-search
    set i to 1
    search e at end display "s5 end"
        when c (i) = "c" set k to i display "s5 c " k
        when c (i) = "b" set k to i display "s5 b " k
    end-search
    move "a1pqra2xyzb1foob3barc9baz" to w
    search all we at end display "s6 end"
        when wk1 (wi) = "b" and wk2 (wi) = 3 display "s6 " wv (wi) end-search
    search all we at end display "s7 end"
        when wk1 (wi) = "b" and wk2-three (wi) display "s7 " wv (wi) end-search
    search all we at end display "s8 end"
        when wk1 (wi) = "c" and wk2 (wi) = 1 display "s8 " wv (wi) end-search
    move "9075503010" to d
    search all dt at end display "s9 end" when dk (di) = 30 set k to di display "s9 " k end-search
    move "13579" to o(1:5)
    search all oe at end display "s10 end" when ok (oi) = 7 set k to oi display "s10 " k end-search
    search all oe at end display "s11 end" when ok (oi) = 5 set k to oi display "s11 " k end-search
    stop run.
