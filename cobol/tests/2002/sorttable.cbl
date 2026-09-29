*> SORT of a table (2023 14.9.40, format 2): the occurrences put in
*> order in place -- an alphanumeric key, a numeric key descending then
*> an alphanumeric one, the table's own OCCURS KEY when no KEY phrase is
*> written, an OCCURS DEPENDING ON table sorted only as far as its count,
*> a signed numeric key.  Equal keys: sortdups.
*> docs/conformance/sort.md
identification division.
program-id. sorttable.
data division.
working-storage section.
01 t1.
   05 e1 occurs 6 ascending key k1.
      10 k1 pic x(3).
      10 v1 pic 9.
01 n2 pic 9 value 4.
01 t2.
   05 e2 occurs 1 to 6 depending on n2.
      10 k2 pic s9(3).
      10 c2 pic x.
01 i pic 9.
procedure division.
    move "pea1fig2ant3fox4bee5asp6" to t1
    sort e1 ascending k1
    display "k1:     " t1
    move "pea1fig2ant3fox4bee5asp6" to t1
    sort e1 descending v1
    display "v1 desc:" t1
    move "pea1fig2ant3fox4bee5asp6" to t1
    sort e1
    display "own key:" t1
    move "pea1fig2ant3fox4bee5asp6" to t1
    sort e1 descending k1 ascending v1
    display "k1 d v1:" t1
    move 12 to k2 (1) move "a" to c2 (1)
    move -5 to k2 (2) move "b" to c2 (2)
    move 7 to k2 (3) move "c" to c2 (3)
    move -9 to k2 (4) move "d" to c2 (4)
    move 6 to n2
    move 1 to k2 (5) move "e" to c2 (5)
    move 0 to k2 (6) move "f" to c2 (6)
    move 4 to n2
    sort e2 ascending k2
    move 6 to n2
    perform varying i from 1 by 1 until i > 6
        display k2 (i) " " c2 (i)
    end-perform
    stop run.
