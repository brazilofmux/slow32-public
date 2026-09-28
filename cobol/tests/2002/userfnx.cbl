identification division.
program-id. userfnx.
*> Functions from another source (tests/2002/lib/fnlib.cbl, named in
*> userfnx.link): the caller knows their signatures from the external
*> repository, the .s32fn files compile.sh's first pass writes (cobol
*> ISSUES-50; docs/functions.md).  Literal arguments go BY CONTENT, as
*> for COMPUTE; GnuCOBOL 4 misreads them (userfnx.oracle-expected: 42
*> arrives as 4200 and clamps to 100).
environment division.
configuration section.
repository.
    function clamp
    function label-of.
data division.
working-storage section.
01  x        pic s9(5) value 250.
01  lo       pic s9(5) value 0.
01  hi       pic s9(5) value 100.
01  n        pic 9 value 2.
procedure division.
main.
    display "clamp(250) = " clamp(x lo hi)
    display "clamp(-5)  = " clamp(-5 lo hi)
    display "clamp(42)  = " clamp(42, 0, 100)
    display "label-of(2) = [" label-of(n) "]"
    display "label-of(7) = [" label-of(7) "]"
    stop run.
end program userfnx.
