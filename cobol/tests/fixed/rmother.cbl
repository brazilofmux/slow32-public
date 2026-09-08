       IDENTIFICATION DIVISION.
       PROGRAM-ID. RMOTHER.
      * no oracle: RM/COBOL 2 names -- OTHER as a data item (PAACEMP),
      * END PROGRAM as the last line with no period (CRPACHK).
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  OTHER   PIC 9(3) VALUE 0.
       01  X       PIC 9(3) VALUE 42.
       PROCEDURE DIVISION.
       MAIN.
           MOVE X TO OTHER.
           ADD 1 TO OTHER.
           DISPLAY "OTHER=" OTHER.
           STOP RUN.
       END PROGRAM
