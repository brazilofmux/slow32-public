*> b7 -- GitHub #29 shape (3): arithmetic with an unsigned DISPLAY integer
*> operand, which reached the numeric stack through cob_get_num's digit loop.
identification division.
program-id. b7.
data division.
working-storage section.
01  ws-i pic 9(9) comp value 0.
01  amt  pic 9(9)  value 123456789.
01  qty  pic 9(5)  value 12345.
01  tot  pic s9(11)v99 value 0.
01  cnt  pic 9(7)  value 0.
procedure division.
main-procedure.
    perform varying ws-i from 1 by 1 until ws-i > 56164
        add amt to tot
        add qty to tot
        add 1 to cnt
    end-perform
    goback.
end program b7.
