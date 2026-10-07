identification division.
function-id. pick.
*> User functions in PERFORM VARYING's item subscript, FROM and BY
*> (standard-queue item 8; they were refused): each is evaluated where
*> the standard evaluates it -- the subscript at every set and
*> augmentation (2023 14.9.28.4 rule 12), FROM at every set (rule 7,
*> and an AFTER item's reset), BY at every augmentation -- so each
*> function shows when it is called.  GnuCOBOL 4 agrees but for the
*> order of the two calls at the first set (FROM before the subscript;
*> docs/oracles.md, fnvarying.oracle-expected).
data division.
linkage section.
01  r pic 9.
procedure division returning r.
    display "  pick"
    move 1 to r
    goback.
end function pick.

identification division.
function-id. base.
data division.
linkage section.
01  r pic 9.
procedure division returning r.
    display "  base"
    move 1 to r
    goback.
end function base.

identification division.
function-id. step.
data division.
linkage section.
01  r pic 9.
procedure division returning r.
    display "  step"
    move 1 to r
    goback.
end function step.

identification division.
program-id. fnvarying.
environment division.
configuration section.
repository.
    function pick
    function base
    function step.
data division.
working-storage section.
01  t.
    05  i pic 9 occurs 3.
01  j pic 9.
procedure division.
    display "subscript, from, by:"
    perform varying i(pick()) from base() by step() until i(1) > 2
        display "body " i(1)
    end-perform
    display "after, with test after:"
    perform with test after varying j from 1 by 1 until j = 2
                            after i(1) from base() by step() until i(1) = 2
        display "body " j " " i(1)
    end-perform
    stop run.
end program fnvarying.
