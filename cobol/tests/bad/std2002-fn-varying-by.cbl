identification division.
function-id. step.
*> VARYING's BY is evaluated at every step, not where it is parsed; a
*> user function there is refused until it is deferred like a
*> condition's (cobol ISSUES-50).
data division.
linkage section.
01  r pic 9.
procedure division returning r.
    move 1 to r
    goback.
end function step.
identification division.
program-id. varyby.
environment division.
configuration section.
repository.
    function step.
data division.
working-storage section.
01  i pic 9.
procedure division.
    perform varying i from 1 by step() until i > 3
        display i
    end-perform
    stop run.
end program varyby.
