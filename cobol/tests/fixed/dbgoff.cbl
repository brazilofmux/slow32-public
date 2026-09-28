       IDENTIFICATION DIVISION.
       PROGRAM-ID. DBGOFF.
      * Without WITH DEBUGGING MODE the debugging lines, D in column
      * 7, are comments (X3.23-1985 VI-10, SOURCE-COMPUTER rule 5;
      * cobol ISSUES-47).  dbgmode.cbl is the same with the clause.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SLOW32.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  N PIC 99 VALUE 1.
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY "START".
      D    DISPLAY "DEBUG " N.
      D    ADD 10
      D        TO N.
           DISPLAY "N=" N.
           STOP RUN.
