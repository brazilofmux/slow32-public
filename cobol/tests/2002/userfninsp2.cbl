identification division.
function-id. dotof.
*> returns its argument's first character; counts the dots in it on the way
data division.
working-storage section.
01  nd       pic 9(3) value 0.
linkage section.
01  x        pic x(3).
01  r        pic x(1).
procedure division using x returning r.
    move 0 to nd
    inspect x tallying nd for all "."
    move x(1:1) to r
    goback.
end function dotof.

identification division.
program-id. userfninsp2.
*> As 2002/userfninsp, for INSPECT REPLACING and CONVERTING: a pattern
*> that is a user function's value, the function itself doing an
*> INSPECT.  The replacing did nothing at all before the statement was
*> read whole (cobol ISSUES-121).  No oracle: GnuCOBOL refuses a user
*> function as a REPLACING or CONVERTING operand ("unexpected user
*> function name"); reviewed by hand.
environment division.
configuration section.
repository.
    function dotof.
data division.
working-storage section.
01  s        pic x(12) value "a.b.c.d.e.f.".
01  k        pic 9(3) value 0.
01  k2       pic 9(3) value 0.
01  t        pic x(12).
01  u        pic x(20) value spaces.
01  p1       pic x(4).
01  p2       pic x(4).
procedure division.
main.
    move s to t
    inspect t replacing all function dotof(".z.") by "-"
    display "replaced: " t
    move s to t
    inspect t replacing all "a" by "A" all function dotof(".z.") by "+"
    display "replaced 2: " t
    move s to t
    inspect t converting function dotof(".q.") to "*"
    display "converted: " t
    stop run.
end program userfninsp2.
