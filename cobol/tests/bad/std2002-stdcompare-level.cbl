*> STANDARD-COMPARE with a literal level the table has not (2023 15.85.4 rule 2): never served, so refused
identification division.
program-id. stdlevel.
procedure division.
    display function standard-compare("a" "b" 5)
    stop run.
