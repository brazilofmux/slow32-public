identification division.
program-id. stopstatus.
*> STOP RUN WITH ERROR STATUS identifier (2002 14.8.38; 2023 14.9.42):
*> the item's value is the exit status passed to the operating system --
*> 4 here (stopstatus.exitcode; the harness checks it).  ERROR alone is
*> 1, NORMAL alone 0, an integer literal its value, an alphanumeric one
*> written to standard error, as GnuCOBOL has them.  The phrase was
*> refused ("'with' is not a COBOL verb") before the sweep of 2026-09-30.
data division.
working-storage section.
01 st pic 9 value 4.
procedure division.
    display "before"
    stop run with error status st.
    display "not reached".
