identification division.
program-id. basedlocal is recursive.
*> A BASED entry in LOCAL-STORAGE (2023 13.18.5; standard-queue item
*> 11): its implicit pointer NULL at each activation (general rule 2;
*> 8.6.5), each activation's its own.  GnuCOBOL 4 keeps the outer
*> activation's address in the inner ones (docs/oracles.md).
data division.
working-storage section.
01  w            pic x(5) value "world".
01  depth        pic 9 value 0.
local-storage section.
01  b            pic x(5) based.
procedure division.
    if address of b = null display "depth " depth ": b is NULL" end-if
    set address of b to address of w
    display "depth " depth ": b = " b
    add 1 to depth
    if depth < 3 call "basedlocal" end-if
    goback.
end program basedlocal.
