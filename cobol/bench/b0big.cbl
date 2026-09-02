*> b0 -- the loop alone: PERFORM VARYING plus one ADD, no I/O, no MOVE.
identification division.
program-id. b0big.
data division.
working-storage section.
01  ws-i     pic 9(9) comp value 0.
01  ws-count pic 9(9) comp value 0.
procedure division.
main-procedure.
    perform varying ws-i from 1 by 1 until ws-i > 4493120
        add 1 to ws-count
    end-perform
    goback.
end program b0big.
