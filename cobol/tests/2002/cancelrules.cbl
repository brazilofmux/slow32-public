*> CANCEL (X3.23-1985 6.x; 2002 14.8.5; 2023 14.9.5): the next CALL finds
*> the program in its initial state.  Its WORKING-STORAGE was reset; now
*> its open files are closed too (2023 14.9.5.4 rule 9 -- the second OPEN
*> OUTPUT after the CANCEL gets 00, not 41) and the programs it contains
*> are canceled with it (rule 4 -- inner's count starts again).  Neither
*> happened before the sweep of 2026-09-30.
identification division.
program-id. cancelrules.
data division.
working-storage section.
01 k pic 9 value 0.
procedure division.
    call "wr" using k
    call "wr" using k
    display "second call, no cancel: k=" k
    cancel "wr"
    call "wr" using k
    display "after cancel: k=" k
    call "outer"
    call "outer"
    cancel "outer"
    call "outer"
    stop run.
end program cancelrules.
identification division.
program-id. wr.
environment division.
input-output section.
file-control.
    select f assign to "tmp/cancelrules.dat" organization line sequential file status fs.
data division.
file section.
fd f.
01 r pic x(4).
working-storage section.
01 fs pic xx.
01 opened pic 9 value 0.
linkage section.
01 kk pic 9.
procedure division using kk.
    open output f
    display "wr: open status " fs " opened-before=" opened
    move 1 to opened
    move "line" to r write r
    add 1 to kk
    goback.
end program wr.
identification division.
program-id. outer.
data division.
working-storage section.
01 n pic 9 value 0.
procedure division.
    add 1 to n
    call "inner"
    display "outer n=" n
    goback.
identification division.
program-id. inner.
data division.
working-storage section.
01 m pic 9 value 0.
procedure division.
    add 1 to m
    display "inner m=" m
    goback.
end program inner.
end program outer.
