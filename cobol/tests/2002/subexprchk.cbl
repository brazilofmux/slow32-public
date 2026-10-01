identification division.
program-id. subexprchk.
*> An arithmetic-expression subscript under EC-BOUND-SUBSCRIPT checking
*> (2023 8.4.2.3.4 rule 2): in range it selects the element; a value
*> that is not an integer (3 / 2) is out of range, as SET's
*> arithmetic-expression is (past the maximum: ecbound, the same check).
*> The raise ends the run after the declarative (fatal), so the last
*> case is the one that raises.  No oracle (ecraise).
data division.
working-storage section.
01  tbl.
    05 elem  pic x(3) occurs 5.
01  w        pic x(3).
01  k        pic 99 value 3.
procedure division.
declaratives.
bd section.
    use after exception condition ec-bound.
b1.
    display "declarative: " function exception-status
            " in " function exception-statement(1:4).
end declaratives.
main section.
m1.
    move "aaa" to elem(1) move "bbb" to elem(2) move "ccc" to elem(3)
>>TURN EC-BOUND CHECKING ON WITH LOCATION
    move elem(k - 1) to w
    display "elem(k - 1) = " w
    move elem(k * 2 - 4) to w
    display "elem(k * 2 - 4) = " w
    move elem(k / 2) to w
    display "not reached"
    stop run.
