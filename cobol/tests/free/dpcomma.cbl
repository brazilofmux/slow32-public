       IDENTIFICATION DIVISION.
       PROGRAM-ID. DPCOMMA.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  N1        PIC 9(8)V99 VALUE 12345678,91.
       01  N2        PIC S9(5)V999 VALUE -12,5.
       01  E1        PIC ZZ.ZZZ.ZZZ,99.
       01  E2        PIC -.---.--9,999.
       01  E3        PIC $$$.$$9,99.
       01  B1        PIC 9(3)V99.
       PROCEDURE DIVISION.
       MAIN.
           MOVE N1 TO E1.
           MOVE N2 TO E2.
           MOVE 1234,5 TO E3.
           DISPLAY '[' E1 '] [' E2 '] [' E3 ']'.
           MOVE E3 TO B1.
           DISPLAY 'B1=' B1 ' N2=' N2.
           COMPUTE B1 = N2 * -2 + 0,25.
           DISPLAY 'B1=' B1.
           IF B1 > 25,2 DISPLAY 'greater' ELSE DISPLAY 'not greater'.
           DISPLAY 3,75 ' ' -0,5.
           STOP RUN.
