identification division.
program-id. valid.
*> VALIDATE is refused by ruling: obsolete in 2023, implemented by no provider.
data division.
working-storage section.
01  x pic x.
procedure division.
    validate x
    stop run.
