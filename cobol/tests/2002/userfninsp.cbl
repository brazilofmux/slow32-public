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
function-id. joinof.
*> returns its argument doubled, built with STRING
data division.
linkage section.
01  x        pic x(2).
01  r        pic x(4).
procedure division using x returning r.
    string x x delimited by size into r
    goback.
end function joinof.

identification division.
program-id. userfninsp.
*> A user function among the operands of INSPECT, STRING and UNSTRING
*> that itself uses the same verb (cobol ISSUES-121).  INSPECT told the
*> runtime of its item, then read its phrases, calling the function in
*> between -- whose own INSPECT took the runtime's state, so the outer
*> one counted 4 dots for 6, or none.  The statement is read whole now,
*> its functions called first (2023 14.6.4), then the runtime's sequence
*> emitted.  GnuCOBOL 4.0-early-dev loses the outer statement the same
*> way, in INSPECT and in STRING (.oracle-expected, docs/oracles.md).
*> REPLACING and CONVERTING are in 2002/userfninsp2: GnuCOBOL refuses a
*> user function there.
environment division.
configuration section.
repository.
    function dotof
    function joinof.
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
    inspect s tallying k for all function dotof(".x.")
    display "tally of dots: " k
    move 0 to k
    inspect s tallying k for all "a" k2 for all function dotof(".y.")
    display "tally a: " k " dots: " k2
    string "<" function joinof("ab") ">" delimited by size into u
    display "string: " u
    move "abXcd" to u
    unstring u delimited by function dotof("X..") into p1 p2
    display "unstring: " p1 "|" p2
    stop run.
end program userfninsp.
