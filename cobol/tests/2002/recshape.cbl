identification division.
program-id. recshape.
*> What a recursive activation owns (COBOL 2002; cobol ISSUES-49).
*> WALK: a PERFORM ... TIMES whose body calls WALK again; each
*> activation's counter is its own, so level 0 runs its loop twice, each
*> of those runs level 1's twice: 1 + 2 + 4 = 7 activations.
*> HOLD: passes a LOCAL-STORAGE item BY REFERENCE to PEEK, which calls
*> HOLD again; the inner HOLD sets its own copy, and PEEK still sees the
*> outer activation's (2023 8.6.4: each activation its own copy).
data division.
working-storage section.
01  calls    pic 9(3) value 0.
01  lvl      pic 9 value 0.
procedure division.
main.
    call "walk" using lvl calls
    display "walk activations: " calls
    move 0 to lvl
    call "hold" using lvl
    stop run.
end program recshape.

identification division.
program-id. walk is recursive.
data division.
local-storage section.
01  next-lvl pic 9.
linkage section.
01  level    pic 9.
01  n        pic 9(3).
procedure division using level n.
w1.
    add 1 to n
    if level < 2
        compute next-lvl = level + 1
        perform 2 times
            call "walk" using next-lvl n
        end-perform
    end-if
    exit program.
end program walk.

identification division.
program-id. hold is recursive.
data division.
local-storage section.
01  mine     pic x(5) value spaces.
linkage section.
01  depth    pic 9.
procedure division using depth.
h1.
    if depth = 0
        move "outer" to mine
    else
        move "inner" to mine
    end-if
    display "hold " depth " sets " mine
    if depth = 0
        call "peek" using mine depth
    end-if
    display "hold " depth " still " mine
    exit program.
end program hold.

identification division.
program-id. peek.
data division.
working-storage section.
01  one      pic 9 value 1.
linkage section.
01  seen     pic x(5).
01  d        pic 9.
procedure division using seen d.
k1.
    display "peek before: " seen
    call "hold" using one
    display "peek after:  " seen
    exit program.
end program peek.
