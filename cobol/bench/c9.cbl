identification division.
program-id. c9.
data division.
working-storage section.
01  ws-i pic 9(9) comp value 0.
01  big  pic s9(13)v99 value 12345678901.23.
01  dst  pic s9(11)v99 value 0.
procedure division.
main-procedure.
    perform varying ws-i from 1 by 1 until ws-i > 56164
        move big to dst
    end-perform
    goback.
end program c9.
