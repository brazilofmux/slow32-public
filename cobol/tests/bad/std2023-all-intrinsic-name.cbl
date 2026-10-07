identification division.
program-id. p-std2023-all-intrinsic-name.
*> FUNCTION ALL INTRINSIC reserves the 2023 function names (12.3.8.3 rule 12; E.2 item 13).
environment division.
configuration section.
repository.
    function all intrinsic.
data division.
working-storage section.
01 concat pic x.
procedure division.
    display concat.
    stop run.
