identification division.
program-id. p-all-intrinsic-name.
*> FUNCTION ALL INTRINSIC: an intrinsic function name is no user-defined word in its scope (2023 12.3.8.3 rule 12); the 2014 functions' too (2014 E.2 item 13).
environment division.
configuration section.
repository.
    function all intrinsic.
data division.
working-storage section.
01 trim pic x(3) value "zzz".
procedure division.
    display "x"
    stop run.
