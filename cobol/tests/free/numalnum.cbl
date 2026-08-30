       IDENTIFICATION DIVISION.
       PROGRAM-ID. NUMALNUM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  N1           PIC 9(3)V99 VALUE 123.45.
       01  N2           PIC S9(2)V9 VALUE -7.5.
       01  N3           PIC 9V9(3) COMP VALUE 0.125.
       01  A            PIC X(8).
       01  B            PIC X(3).
       PROCEDURE DIVISION.
       MAIN.
           MOVE N1 TO A.
           DISPLAY '[' A ']'.
           MOVE N2 TO A.
           DISPLAY '[' A ']'.
           MOVE N3 TO A.
           DISPLAY '[' A ']'.
           MOVE N1 TO B.
           DISPLAY '[' B ']'.
           STOP RUN.
