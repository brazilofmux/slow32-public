       IDENTIFICATION DIVISION.
      * no oracle: a GO TO out of a performed range, repeated as an
      * operator command would repeat it (GLENTER's S command); the
      * abandoned frames must not accumulate.
       PROGRAM-ID. PERFGOTO.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  N   PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
       MAIN.
       LOOP.
           ADD 1 TO N.
           IF N > 300 DISPLAY "300 abandoned ranges survived" STOP RUN.
           PERFORM GET-ENTRY THRU GET-EXIT.
           DISPLAY "not reached".
           STOP RUN.
       GET-ENTRY.
           GO TO LOOP.
       GET-EXIT.
           EXIT.
