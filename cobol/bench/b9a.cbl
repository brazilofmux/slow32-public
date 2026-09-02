*> b9a -- the issue's headline ADD, exactly: PIC 9(9)V99 TO PIC S9(11)V99.
identification division.
program-id. b9a.
data division.
working-storage section.
01  ws-i pic 9(9) comp value 0.
01  amt  pic 9(9)v99  value 1234567.89.
01  tot  pic s9(11)v99 value 0.
procedure division.
main-procedure.
    perform varying ws-i from 1 by 1 until ws-i > 56164
        add amt to tot
    end-perform
    goback.
end program b9a.
