*> LOCALE-COMPARE's third argument is a locale-name of SPECIAL-NAMES, not a string (2023 15.51.3 rule 4; GnuCOBOL's string form is not taken)
identification division.
program-id. loccmpstr.
procedure division.
    display function locale-compare("a" "b" "sv_SE")
    stop run.
