IDENTIFICATION DIVISION.
PROGRAM-ID. CALLEXC.
*> CALL of a program that is not there: ON EXCEPTION runs, and then
*> control goes to the end of the CALL -- NOT ON EXCEPTION does not run
*> as well (2023 14.9.4.4 general rule 3h1).
*> GnuCOBOL 4.0-early-dev runs both when the ON EXCEPTION phrase falls
*> through (.oracle-expected; docs/oracles.md).  tests/gen/gen-flow.py
*> found it.
DATA DIVISION.
WORKING-STORAGE SECTION.
01  N PIC 9 VALUE 0.
PROCEDURE DIVISION.
MAIN.
    CALL "NOSUCHPROG" ON EXCEPTION DISPLAY "on exception"
        NOT ON EXCEPTION DISPLAY "not on exception" END-CALL
    CALL "NOSUCHPROG" ON EXCEPTION ADD 1 TO N END-CALL
    DISPLAY "exceptions counted: " N
    STOP RUN.
