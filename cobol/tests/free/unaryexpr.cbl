       IDENTIFICATION DIVISION.
       PROGRAM-ID. UNARYEXPR.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  NINE         PIC 9 VALUE 9.
       01  SEVEN        PIC 9 VALUE 7.
       01  N            PIC S9(3) VALUE -5.
       PROCEDURE DIVISION.
       MAIN.
           IF NINE * 9 - 7 * SEVEN NOT EQUAL - (SEVEN * 7) + 9 * NINE
               DISPLAY 'differ' ELSE DISPLAY 'same'.
           IF - N = 5 DISPLAY 'unary minus ok'.
           IF + N < - 4 DISPLAY 'unary plus ok'.
           IF - (N + 1) = 4 AND - N - 1 = 4 DISPLAY 'both forms ok'.
           STOP RUN.
