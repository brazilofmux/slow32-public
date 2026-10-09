identification division.
program-id. chnat.
*> A 2002 function whose module is not here yet is refused naming it
*> (LOCALE-DATE until docs/plans/locale.md step 2; LOCALE-COMPARE came with step 1).
procedure division.
    display function locale-date("20261008")
    stop run.
