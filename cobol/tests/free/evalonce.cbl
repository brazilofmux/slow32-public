IDENTIFICATION DIVISION.
PROGRAM-ID. EVALONCE.
*> An EVALUATE subject that is an arithmetic expression or a numeric
*> function is assigned its value once, at the beginning (2023 14.9.13.4
*> rule 3c; X3.23-1985 EVALUATE general rule 1), and every WHEN compares
*> against that value.  It was evaluated again for each WHEN, twice for a
*> THRU: a die rolled with FUNCTION RANDOM matched none of WHEN 1 ...
*> WHEN 6 about a third of the time (cobol ISSUES-121).  GnuCOBOL
*> 4.0-early-dev rolls again for each WHEN too (.oracle-expected,
*> docs/oracles.md).
DATA DIVISION.
WORKING-STORAGE SECTION.
01  I        PIC 9(3).
01  X        PIC 9V9(6).
01  A        PIC S9(3) VALUE 7.
01  B        PIC S9(3)V99 VALUE 2.50.
01  HITS     PIC 9(3) VALUE 0.
01  NONE-HIT PIC 9(3) VALUE 0.
PROCEDURE DIVISION.
MAIN.
    COMPUTE X = FUNCTION RANDOM(7)
    PERFORM VARYING I FROM 1 BY 1 UNTIL I > 600
        EVALUATE FUNCTION INTEGER(FUNCTION RANDOM * 6) + 1
            WHEN 1 ADD 1 TO HITS
            WHEN 2 ADD 1 TO HITS
            WHEN 3 THRU 4 ADD 1 TO HITS
            WHEN 5 ADD 1 TO HITS
            WHEN 6 ADD 1 TO HITS
            WHEN OTHER ADD 1 TO NONE-HIT
        END-EVALUATE
    END-PERFORM
    DISPLAY "600 rolls: " HITS " on a face, " NONE-HIT " on none"
    EVALUATE A + 3
        WHEN 9 DISPLAY "a + 3: nine"
        WHEN 10 THRU 12 DISPLAY "a + 3: ten to twelve"
        WHEN OTHER DISPLAY "a + 3: other"
    END-EVALUATE
    EVALUATE A * B
        WHEN 17 DISPLAY "a * b: 17"
        WHEN 17.5 DISPLAY "a * b: 17.5"
        WHEN OTHER DISPLAY "a * b: other"
    END-EVALUATE
    EVALUATE A / 4
        WHEN 1.7 DISPLAY "a / 4: 1.7"
        WHEN 1.75 DISPLAY "a / 4: 1.75"
        WHEN OTHER DISPLAY "a / 4: other"
    END-EVALUATE
    EVALUATE FUNCTION MOD(A, 4) ALSO A - 10
        WHEN 3 ALSO -5 THRU -1 DISPLAY "mod and difference: 3, -3"
        WHEN OTHER DISPLAY "mod and difference: other"
    END-EVALUATE
    STOP RUN.
