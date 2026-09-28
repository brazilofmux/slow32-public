identification division.
program-id. trim14.
*> TRIM arrived in COBOL 2014; -std=2002 names the edition it needs.
procedure division.
    display function trim("  x  ")
    stop run.
