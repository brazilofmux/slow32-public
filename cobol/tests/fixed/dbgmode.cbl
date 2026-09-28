       IDENTIFICATION DIVISION.
       PROGRAM-ID. DBGMODE.
      * WITH DEBUGGING MODE compiles the debugging lines, D in column
      * 7 (X3.23-1985 VI-10, SOURCE-COMPUTER rules 4-5; cobol
      * ISSUES-47).  dbgoff.cbl is the same program without the clause.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SLOW32 WITH DEBUGGING MODE.
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
