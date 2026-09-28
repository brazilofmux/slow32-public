identification division.
program-id. recfact.
*> RECURSIVE and LOCAL-STORAGE (COBOL 2002; cobol ISSUES-49): factorial by
*> a program that calls itself.  Each activation has its own LOCAL-STORAGE,
*> set to its VALUEs on entry (DEPTH is 1 in every one), its own LINKAGE
*> (K is still this activation's after the inner CALL returns), and its
*> own TIMES counter.
data division.
working-storage section.
01  n        pic 9(2) value 5.
01  r        pic 9(9).
procedure division.
main.
    call "factr" using n r
    display "5! = " r
    move 10 to n
    call "factr" using n r
    display "10! = " r
    stop run.
end program recfact.
identification division.
program-id. factr is recursive.
data division.
local-storage section.
01  m        pic 9(2).
01  sub-r    pic 9(9).
01  depth    pic 9(2) value 0.
linkage section.
01  k        pic 9(2).
01  res      pic 9(9).
procedure division using k res.
p1.
    add 1 to depth
    if k <= 1
        move 1 to res
    else
        compute m = k - 1
        perform 2 times
            continue
        end-perform
        call "factr" using m sub-r
        compute res = k * sub-r
    end-if
    display "k=" k " depth=" depth " res=" res
    exit program.
end program factr.
