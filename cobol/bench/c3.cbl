identification division.
program-id. c3.
data division.
working-storage section.
01  ws-i pic 9(9) comp value 0.
01  a3 pic 9(9)v99 value 123.45.
01  b3 pic s9(11)v99 value 0.
procedure division.
main-procedure.
    perform varying ws-i from 1 by 1 until ws-i > 56164
        add a3 to b3
        add a3 to b3
        add a3 to b3
        add a3 to b3
    end-perform
    goback.
end program c3.
