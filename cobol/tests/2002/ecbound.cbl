identification division.
program-id. ecbound.
*> EC-BOUND-SUBSCRIPT (COBOL 2002 8.4.2.3.4 rule 2; cobol ISSUES-56): a
*> subscript outside 1 through the OCCURS maximum.  Unchecked, a store to
*> elem(9) of a five-element table lands in the item that follows (S);
*> checked, elem(6) raises the condition before the MOVE touches
*> anything, and the run ends after the declarative (fatal).  WITH
*> LOCATION names the statement.  No oracle (ecraise).
data division.
working-storage section.
01  tbl.
    05 elem  pic x(3) occurs 5 indexed by ix.
01  s        pic x(10) value "abcdefghij".
01  w        pic x(3).
01  k        pic 99.
procedure division.
declaratives.
bd section.
    use after exception condition ec-bound.
b1.
    display "declarative: " function exception-status
            " in " function exception-statement(1:4) " w=" w.
end declaratives.
main section.
m1.
    move 9 to k  move "zzz" to elem(k)
    display "unchecked elem(9) landed in s: " s
>>TURN EC-BOUND CHECKING ON WITH LOCATION
    move 5 to k  move elem(k) to w
    display "elem(5) = [" w "]"
    set ix to 6
    move elem(ix) to w
    display "not reached"
    stop run.
end program ecbound.
