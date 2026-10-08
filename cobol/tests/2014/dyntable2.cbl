identification division.
program-id. dyntable2.
*> Dynamic-capacity tables, the rest (2023 8.5.1.9): a group element with a
*> fixed table and a condition-name inside it, reference modification of
*> an element, STRING INTO and ADD TO as the receiving operands that make
*> an element; a table in LINKAGE, its slot the caller's; a table in a
*> called program's WORKING-STORAGE that CANCEL puts back to its initial
*> state (the elements given up, the capacity the minimum).  No oracle:
*> GnuCOBOL 4 has no OCCURS DYNAMIC.  No gcobol either.
data division.
working-storage section.
01 g.
   05 t occurs dynamic capacity in cap initialized.
      10 name-x pic x(6) value "nobody".
      10 kind pic 9.
         88 is-big value 2 thru 9.
      10 score pic 9(3) occurs 3 value 5.
   05 total pic 9(5) value 0.
01 i pic 9(4).
01 p pic 9(4).
01 w pic x(12).
procedure division.
m1.
    move "alice" to name-x(1). move 2 to kind(1). move 10 to score(1, 2).
    move "bob" to name-x(2). move 1 to kind(2).
    display "cap=" cap " " name-x(1) " " kind(1) " " score(1, 1) score(1, 2) score(1, 3) " / " name-x(2) " " kind(2) " " score(2, 3).
    if is-big(1) display "alice is big" end-if.
    if not is-big(2) display "bob is not" end-if.
    move "XY" to name-x(3)(2:2).
    display "refmod: [" name-x(3) "] cap=" cap.
    move 1 to p.
    string "car" "ol" delimited by size into name-x(4) with pointer p.
    display "string: [" name-x(4) "] p=" p " cap=" cap.
    add 7 to score(5, 1).
    display "add: " score(5, 1) " cap=" cap.
    perform varying i from 1 by 1 until i > cap add kind(i) to total end-perform.
    display "sum of kinds: " total.
    call "dyn2sub" using g.
    display "after sub: cap=" cap " " name-x(6) " total=" total.
    call "dyn2own".
    call "dyn2own".
    cancel "dyn2own".
    call "dyn2own".
    display "done".
    stop run.
identification division.
program-id. dyn2sub.
data division.
linkage section.
01 lg.
   05 lt occurs dynamic capacity in lcap.
      10 lname pic x(6).
      10 lkind pic 9.
      10 lscore pic 9(3) occurs 3.
   05 ltotal pic 9(5).
procedure division using lg.
    display "  sub: lcap=" lcap " " lname(1) " " lscore(1, 2).
    move "frank" to lname(6).
    move 99 to ltotal.
    display "  sub stored 6: lcap=" lcap.
    goback.
end program dyn2sub.
identification division.
program-id. dyn2own.
data division.
working-storage section.
01 og.
   05 ot pic 9(2) occurs dynamic capacity in ocap from 1.
   05 calls pic 9 value 0.
procedure division.
    add 1 to calls.
    add 1 to ot(1).
    move calls to ot(calls + 1).
    display "  own: call " calls " ocap=" ocap " ot(1)=" ot(1) " ot(2)=" ot(2).
    goback.
end program dyn2own.
end program dyntable2.
