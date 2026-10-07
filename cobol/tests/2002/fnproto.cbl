identification division.
function-id. scale as "fn-scale" is prototype.
*> The rest of the CALL family for functions (docs/plans/standard-queue.md
*> item 8): a prototype (11.5 format 2) ahead of its definition, which
*> must conform; AS literal, the externalized name, on the FUNCTION-ID and
*> in the REPOSITORY; BY VALUE parameters, converted as COMPUTE would
*> (14.8.2.3.3 rule 2a) -- so scale(7, 1.5) sees 1.5 in a V9 parameter and
*> 7 in a 9(3) one, whatever the argument's picture; OPTIONAL parameters
*> OMITTED or left off the end (14.8.2.1), tested with IS OMITTED; and
*> twelve parameters, past the eight argument registers.  No oracle:
*> GnuCOBOL 4 does not implement function prototypes.
data division.
linkage section.
01  n        pic 9(3).
01  f        pic 9v9.
01  r        pic 9(5)v9.
procedure division using by value n f returning r.
end function scale.

identification division.
function-id. scale as "fn-scale".
data division.
linkage section.
01  n        pic 9(3).
01  f        pic 9v9.
01  r        pic 9(5)v9.
procedure division using by value n f returning r.
    compute r = n * f
    goback.
end function scale.

identification division.
function-id. opt-sum.
data division.
linkage section.
01  a        pic 9(3).
01  b        pic 9(3).
01  c        pic 9(3).
01  r        pic 9(5).
procedure division using a optional b optional c returning r.
    move a to r
    if b is not omitted add b to r end-if
    if c is not omitted add c to r end-if
    goback.
end function opt-sum.

identification division.
function-id. twelve.
data division.
linkage section.
01  p1       pic 9(2).
01  p2       pic 9(2).
01  p3       pic 9(2).
01  p4       pic 9(2).
01  p5       pic 9(2).
01  p6       pic 9(2).
01  p7       pic 9(2).
01  p8       pic 9(2).
01  p9       pic 9(2).
01  p10      pic 9(2).
01  p11      pic 9(2).
01  p12      pic 9(2).
01  r        pic 9(4).
procedure division using p1 p2 p3 p4 p5 p6 p7 p8 by value p9 p10 p11 p12 returning r.
    compute r = p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8
              + 100 * p9 + 100 * p10 + 100 * p11 + 100 * p12
    goback.
end function twelve.

identification division.
program-id. fnproto.
environment division.
configuration section.
repository.
    function sc as "fn-scale"
    function opt-sum
    function twelve.
data division.
working-storage section.
01  i        pic s9(4) value 7.
01  d        pic 9(4)v99 value 2.50.
01  w        pic 9(2) value 1.
01  k        pic 9(3) value 9.
procedure division.
    display "scale 7 1.5:   " sc(7, 1.5)
    display "scale i d:     " sc(i, d)
    display "scale i+1 d/2: " sc(i + 1, d / 2)
    display "opt-sum 1:     " opt-sum(1)
    display "opt-sum 1 2:   " opt-sum(1, 2)
    display "opt-sum 1 2 3: " opt-sum(1, 2, 3)
    display "opt-sum 1 _ 3: " opt-sum(1, omitted, 3)
    display "twelve:        " twelve(w, w, w, w, w, w, w, w, 1, k, 2, 3)
    stop run.
end program fnproto.
