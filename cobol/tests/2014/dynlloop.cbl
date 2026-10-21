identification division.
program-id. dynlloop.
*> A level-01 DYNAMIC LENGTH item gives its heap back: LOCAL-STORAGE's
*> at every GOBACK, WORKING-STORAGE's at CANCEL (14.9.5.4 rule 2).  Two
*> hundred CALLs of each kind, every one holding a 400,000-character
*> copy, are 80 million characters if none is freed -- more than the 64M
*> heap a program has here, so a leak cannot finish.  (The item is its
*> own record, which the frees once skipped: they looked only below it.)
*> No oracle: GnuCOBOL 4 has no DYNAMIC LENGTH.  No gcobol either.
data division.
working-storage section.
01 big pic x(400000).
01 i pic 9(3).
01 total pic 9(9) value 0.
procedure division.
    move all "q" to big
    perform varying i from 1 by 1 until i > 200
        call "dlhold" using big total
    end-perform
    display "local-storage: 200 calls, characters held in turn " total
    move 0 to total
    perform varying i from 1 by 1 until i > 200
        call "dlkeep" using big total
        cancel "dlkeep"
    end-perform
    display "cancel: 200 calls, characters held in turn " total
    stop run.
end program dynlloop.

identification division.
program-id. dlhold.
data division.
local-storage section.
01 s pic x dynamic length.
linkage section.
01 v pic x(400000).
01 t pic 9(9).
procedure division using v t.
    move v to s
    add function length(s) to t
    goback.
end program dlhold.

identification division.
program-id. dlkeep.
data division.
working-storage section.
01 s pic x dynamic length.
linkage section.
01 v pic x(400000).
01 t pic 9(9).
procedure division using v t.
    move v to s
    add function length(s) to t
    goback.
end program dlkeep.
