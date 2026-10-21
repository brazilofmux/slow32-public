identification division.
program-id. dynlself.
*> A MOVE of a dynamic-length item onto itself, or of a reference-modified
*> slice of it onto the whole item: the sender's bytes are kept across the
*> realloc.  A variable-length group moved onto itself is a no-op.  No
*> oracle: GnuCOBOL 4 has no DYNAMIC LENGTH.  No gcobol either.
data division.
working-storage section.
01 s pic x dynamic length.
01 g.
   05 h pic x(2) value "aa".
   05 t occurs dynamic capacity in c.
      10 k pic 9(2).
   05 s2 pic x dynamic length.
   05 z pic x(2) value "zz".
procedure division.
    move "abcdef" to s
    move s to s
    display "self: [" s "] " function length(s)
    move s(1:3) to s
    display "prefix: [" s "] " function length(s)
    move "xyzxyzxyz" to s
    move s(4:3) to s
    display "mid: [" s "] " function length(s)
    move 11 to k(1)
    move 22 to k(2)
    move "hello" to s2
    move g to g
    display "group self: " h " c=" c " " k(1) k(2) " s2=[" s2 "] " z
    stop run.
