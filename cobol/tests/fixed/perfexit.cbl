       IDENTIFICATION DIVISION.
       PROGRAM-ID. PERFEXIT.
      * no oracle: a GO TO out of a performed paragraph into the
      * enclosing range's exit (Open Systems PAPOST 745 -> 750 inside
      * PERFORM 705 THRU 750).  The range must still return.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  N   PIC 9 VALUE 0.
       PROCEDURE DIVISION.
       MAIN.
           PERFORM A THRU A-EXIT.
           DISPLAY "back in MAIN".
           STOP RUN.
       B.
           DISPLAY "B".
           GO TO A-EXIT.
       A.
           DISPLAY "A".
           PERFORM B.
           DISPLAY "not reached".
       A-EXIT.
           EXIT.
       Z.
           DISPLAY "FELL INTO Z".
           STOP RUN.
