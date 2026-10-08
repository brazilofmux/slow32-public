*> A screen placed AT another origin than line 1, column 1 (2023 14.9.11.3
*> rule 3; standard-queue item 38): every field offset by the AT phrase --
*> AT 0510, AT LINE 7 COLUMN 3, and from an item.  It was refused as not
*> implemented (cobol ISSUES-124). No oracle: screens need a tty.
identification division.
program-id. scratoff.
data division.
working-storage section.
01  pos pic 9(4) value 0902.
screen section.
01  s1.
    03  value "X" line 1 col 1.
    03  value "Y" line 2 col 3.
procedure division.
    display s1 at 0510
    display s1 at line 7 column 3
    display s1 at pos
    stop run.
