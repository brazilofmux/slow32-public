       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEBUGLN.
      * A debugging line without WITH DEBUGGING MODE is a comment, and
      * -warn-74 names it: the Debug module, obsolete item 18 (BP-O12).
       PROCEDURE DIVISION.
       MAIN.
      D    DISPLAY "NOT COMPILED".
           STOP RUN.
