*> b5 -- the comparison shapes of GitHub #29: identical unsigned DISPLAY
*> descriptors of several widths, which is what a join step's key tests
*> and a report's control breaks are made of.
identification division.
program-id. b5big.
data division.
working-storage section.
01  ws-i pic 9(9) comp value 0.
01  a2   pic 9(2)  value 42.
01  b2   pic 9(2)  value 42.
01  a5   pic 9(5)  value 12345.
01  b5f  pic 9(5)  value 12345.
01  a8   pic 9(8)  value 20260501.
01  b8   pic 9(8)  value 20260501.
01  a10  pic 9(10) value 1234567890.
01  b10  pic 9(10) value 1234567890.
01  hits pic 9(9) comp value 0.
procedure division.
main-procedure.
    perform varying ws-i from 1 by 1 until ws-i > 2246560
        if a2  =  b2  add 1 to hits end-if
        if a5  =  b5f add 1 to hits end-if
        if a8  <= b8  add 1 to hits end-if
        if a10 =  b10 add 1 to hits end-if
    end-perform
    goback.
end program b5big.
