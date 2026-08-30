       IDENTIFICATION DIVISION.
       PROGRAM-ID. CURRENCY1.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CURRENCY SIGN IS "W".
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  N            PIC 9(5)V99 VALUE 1234.5.
       01  E1           PIC W(6)9.99.
       01  E2           PIC WWW,WW9.99.
       01  E3           PIC W9(5).99.
       01  E4           PIC WWWWW.
       01  B            PIC 9(5)V99.
       PROCEDURE DIVISION.
       MAIN.
           MOVE N TO E1 E2 E3.
           MOVE 42 TO E4.
           DISPLAY '[' E1 '][' E2 '][' E3 '][' E4 ']'.
           MOVE E2 TO B.
           DISPLAY B.
           MOVE 0 TO E1.
           DISPLAY '[' E1 ']'.
           STOP RUN.
