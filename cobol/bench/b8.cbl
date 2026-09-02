identification division.
program-id. b8.
data division.
working-storage section.
01  ws-i pic 9(9) comp value 0.
01  cnt  pic 9(7) value 0.
procedure division.
main-procedure.
    perform varying ws-i from 1 by 1 until ws-i > 56164
        add 1 to cnt
    end-perform
    goback.
end program b8.
