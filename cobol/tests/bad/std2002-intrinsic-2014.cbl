identification division.
program-id. spm14.
*> SECONDS-PAST-MIDNIGHT arrived in COBOL 2014; -std=2002 names the
*> edition it needs.  (TRIM, also 2014, is taken as BP-E27.)
procedure division.
    display function seconds-past-midnight
    stop run.
