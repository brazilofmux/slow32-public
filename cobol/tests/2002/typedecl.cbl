identification division.
program-id. typedecl.
*> TYPEDEF and TYPE (2023 13.18.58, 13.18.57; cobol ISSUES-79): a type
*> declaration has no storage; TYPE TO puts the type's description in
*> place of the clause -- an elementary type's PICTURE, USAGE and VALUE,
*> a group type's subordinates with their levels adjusted to the entry's
*> (rule 2b) and their own VALUEs and level 88s, an elementary type's
*> level 88 with it (an entry with TYPE is followed by no 88 of its own,
*> 13.18.57.3 rule 2) -- and a type may use a type.  The subordinates are qualified by the group that uses the type
*> (13.18.58.4 rule 1).  The entry's own clauses stay: OCCURS here.
*> No oracle (docs/typedef.md): GnuCOBOL 4 knows no TYPEDEF under
*> -std=cobol2002 and, by default, not a type used inside a type.
data division.
working-storage section.
01  money-t     typedef pic s9(7)v99 comp-3.
01  flag-t      typedef pic x value "n".
    88 done     value "y".
01  point-t     typedef.
    05 px       pic s999 value 0.
    05 py       pic s999 value 0.
01  line-t      is typedef.
    05 from-pt  type to point-t.
    05 to-pt    type point-t.
    05 status-f type to flag-t.
01  total       type to money-t.
01  price       type money-t value 12.50.
01  seg         type to line-t.
01  path.
    05 pts      type to point-t occurs 3.
procedure division.
main.
    display "price: " price " length " function length(price)
    compute total = price * 3
    display "total: " total
    move 1 to px of from-pt  move 2 to py of from-pt
    move 7 to px of to-pt    move -4 to py of to-pt
    display "segment: " px of from-pt "," py of from-pt " to " px of to-pt "," py of to-pt
    display "status: " status-f
    set done to true
    if done display "done" end-if
    display "length of seg: " function length(seg)
    move 5 to px of pts(2)
    display "path: " px of pts(1) " " px of pts(2) " " px of pts(3)
    stop run.
