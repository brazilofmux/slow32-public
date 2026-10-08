*> FROM x TO y in one screen entry (2023 13.17.2, one source-destination
*> clause; standard-queue item 38): shown from a, keyed into b, a left as
*> it was.  No oracle: screens need a tty.
identification division.
program-id. scrfromto.
data division.
working-storage section.
01 a pic x(3) value 'abc'.
01 b pic x(3) value 'zzz'.
screen section.
01 s1.
   05 line 1 col 1 value 'in:'.
   05 col plus 1 pic x(3) from a to b.
procedure division.
    accept s1
    display 'a=[' a '] b=[' b ']'
    goback.
