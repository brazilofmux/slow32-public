identification division.
program-id. area as "pg-area" is prototype.
*> The CALL family's program side (standard-queue item 8): a program
*> prototype (11.10 format 2) with AS literal, the REPOSITORY's
*> program-specifier, CALL by the prototype's name and CALL literal AS
*> prototype-name (14.9.4 format 2), through which BY CONTENT arguments
*> and expressions are converted to the parameter's description
*> (14.8.2.3.3 rule 2: 2.5 reaches a 9V9 parameter as 2.5, whatever the
*> argument's picture), BY VALUE expressions, OPTIONAL parameters left
*> off, RETURNING checked against the signature; and AS NESTED to a
*> contained program.  No oracle: GnuCOBOL 4 does not implement program
*> prototypes.
data division.
linkage section.
01  w        pic 9(3)v9.
01  h        pic 9(3)v9.
01  r        pic 9(7)v99.
procedure division using w h returning r.
end program area.

identification division.
program-id. tally is prototype.
data division.
linkage section.
01  n        pic 9(3).
01  m        pic 9(3).
01  r        pic 9(4).
procedure division using by value n by reference optional m returning r.
end program tally.

identification division.
program-id. pgproto.
environment division.
configuration section.
repository.
    program area as "pg-area"
    program tally.
data division.
working-storage section.
01  a        pic 9(5)v99 value 12.5.
01  b        pic s9(4) value 4.
01  res      pic 9(7)v99.
01  cnt      pic 9(4).
procedure division.
    call area using by content a b returning res
    display "area a b:      " res
    call area using a + 1 b / 2 returning res
    display "area a+1 b/2:  " res
    call "pg-area" as area using 2.5 3.5 returning res
    display "area 2.5 3.5:  " res
    call tally using by value b + 1 returning cnt
    display "tally b+1:     " cnt
    call tally using by value 7 by content 2 returning cnt
    display "tally 7 2:     " cnt
    call "inner" as nested using b
    display "inner b:       " b
    stop run.

identification division.
program-id. inner.
data division.
linkage section.
01  n        pic s9(4).
procedure division using n.
    add 100 to n
    goback.
end program inner.
end program pgproto.

identification division.
program-id. area as "pg-area".
data division.
linkage section.
01  w        pic 9(3)v9.
01  h        pic 9(3)v9.
01  r        pic 9(7)v99.
procedure division using w h returning r.
    compute r = w * h
    goback.
end program area.

identification division.
program-id. tally.
data division.
linkage section.
01  n        pic 9(3).
01  m        pic 9(3).
01  r        pic 9(4).
procedure division using by value n by reference optional m returning r.
    move n to r
    if m is not omitted add m to r end-if
    goback.
end program tally.
