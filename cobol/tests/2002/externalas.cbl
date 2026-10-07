identification division.
program-id. externalas.
*> EXTERNAL AS literal (2023 13.18.22): the externalized name the
*> storage is shared under, so two programs share a record described
*> under different data-names (no VALUE clauses: general rule 6 wants
*> them identical in every description).  GnuCOBOL 4 agrees.
data division.
working-storage section.
01  shared-here external as "the-shared-area".
    05  count-1  pic 9(3).
    05  text-1   pic x(5).
procedure division.
    move 0 to count-1
    move "alpha" to text-1
    add 1 to count-1
    call "other"
    display "here:  " shared-here
    stop run.
end program externalas.
identification division.
program-id. other.
data division.
working-storage section.
01  shared-there external as "the-shared-area".
    05  count-2  pic 9(3).
    05  text-2   pic x(5).
procedure division.
    display "there: " shared-there
    add 10 to count-2
    move "omega" to text-2
    goback.
end program other.
