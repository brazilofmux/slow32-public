IDENTIFICATION DIVISION.
PROGRAM-ID. EVALWIDE.
*> An EVALUATE subject evaluated once and kept (free/evalonce), where
*> the value needs the wide stack: more than 18 digits, floating point,
*> and a narrow subject compared with a wide object -- the kept value
*> goes back onto either stack (cob_nsave keeps it in the wide form;
*> cobol ISSUES-121).
DATA DIVISION.
WORKING-STORAGE SECTION.
01  W        PIC 9(25) VALUE 1234567890123456789012345.
01  F        USAGE FLOAT-LONG VALUE 2.5.
01  N        PIC 9(3) VALUE 7.
PROCEDURE DIVISION.
MAIN.
    EVALUATE W + 1
        WHEN 1234567890123456789012345 DISPLAY "wide: unchanged"
        WHEN 1234567890123456789012346 DISPLAY "wide: plus one"
        WHEN OTHER DISPLAY "wide: other"
    END-EVALUATE
    EVALUATE W * 2
        WHEN 1 THRU 9 DISPLAY "wide product: small"
        WHEN 2469135780246913578024690 DISPLAY "wide product: doubled"
        WHEN OTHER DISPLAY "wide product: other"
    END-EVALUATE
    EVALUATE F * 2
        WHEN 4 DISPLAY "float: four"
        WHEN 5 DISPLAY "float: five"
        WHEN OTHER DISPLAY "float: other"
    END-EVALUATE
    EVALUATE N + W - W
        WHEN 6 DISPLAY "mixed: six"
        WHEN 7 DISPLAY "mixed: seven"
        WHEN OTHER DISPLAY "mixed: other"
    END-EVALUATE
    EVALUATE N + 1
        WHEN 1234567890123456789012345 DISPLAY "narrow against wide: equal"
        WHEN 8 DISPLAY "narrow against wide: eight"
        WHEN OTHER DISPLAY "narrow against wide: other"
    END-EVALUATE
    STOP RUN.
