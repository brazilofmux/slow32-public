*> An index-name of a table in a GLOBAL item is global too (2023
*> 8.4.6.2.3): a contained program SETs and subscripts with it.
*> ACAS's stock DAL (cobol ISSUES-124) indexes its GLOBAL key and
*> repeating-group tables this way from nested programs.
identification division.
program-id. globalidx.
data division.
working-storage section.
01  rg-table  global.
    12 filler pic x(4) value "AAAA".
    12 filler pic x(4) value "BBBB".
01  rgt redefines rg-table global.
    12 rg-entry occurs 2 indexed by rg-x1.
       15 rg-name pic x(4).
procedure division.
    call "inner"
    stop run.
identification division.
program-id. inner.
procedure division.
    set rg-x1 to 2
    display rg-name (rg-x1)
    set rg-x1 down by 1
    display rg-name (rg-x1)
    exit program.
end program inner.
end program globalidx.
