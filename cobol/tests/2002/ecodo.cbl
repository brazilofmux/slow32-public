identification division.
program-id. ecodo.
*> EC-BOUND-ODO (COBOL 2002 13.18.38 general rule 7; cobol ISSUES-61):
*> referring to an OCCURS DEPENDING ON table, an item in it, or a group
*> holding it needs the DEPENDING ON value within the OCCURS bounds.  With
*> N at 3 the group and an element are fine; with N at 7, past OCCURS 2 TO
*> 5, referring to the group raises it.  Fatal.  No oracle (ecraise).
data division.
working-storage section.
01  n        pic 9 value 3.
01  grp.
    05 cnt   pic 99 value 0.
    05 elem  pic x occurs 2 to 5 depending on n.
01  w        pic x(10).
procedure division.
declaratives.
od section.
    use after exception condition ec-bound-odo.
o1.
    display "declarative: " function exception-status " n=" n.
end declaratives.
main section.
m1.
>>TURN EC-BOUND-ODO CHECKING ON
    move "abc" to grp(3:3)
    move grp to w
    display "n=3: [" w "]"
    move elem(2) to w
    display "elem(2): [" w "]"
    move 7 to n
    move grp to w
    display "not reached"
    stop run.
end program ecodo.
