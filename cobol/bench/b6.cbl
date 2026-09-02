*> b6 -- the shape (2) comparisons of GitHub #29: an unsigned DISPLAY
*> integer against a binary item and against a literal, which is what a
*> report's date window and a join step's key range are made of.
identification division.
program-id. b6.
data division.
working-storage section.
01  ws-i pic 9(9) comp value 0.
01  d5   pic 9(5)  value 12345.
01  d8   pic 9(8)  value 20260501.
01  d2   pic 9(2)  value 42.
01  bin  pic s9(9) comp value 20260501.
01  hits pic 9(9) comp value 0.
procedure division.
main-procedure.
    perform varying ws-i from 1 by 1 until ws-i > 56164
        if d8 <= bin    add 1 to hits end-if
        if d5  = 12345  add 1 to hits end-if
        if d2  > 40     add 1 to hits end-if
        if d8  > 20250101 add 1 to hits end-if
    end-perform
    goback.
end program b6.
