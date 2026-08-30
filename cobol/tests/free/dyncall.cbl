       IDENTIFICATION DIVISION.
       PROGRAM-ID. DYNCALL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  PNAME        PIC X(10).
       01  N            PIC 9(4) VALUE 5.
       01  FLAG         PIC X VALUE 'P'.
       PROCEDURE DIVISION.
       MAIN.
           MOVE 'TWICE' TO PNAME.
           CALL PNAME USING N.
           DISPLAY 'after TWICE: ' N.
           CALL PNAME USING N
               ON EXCEPTION MOVE 'F' TO FLAG
               NOT ON EXCEPTION DISPLAY 'called again: ' N
           END-CALL.
           DISPLAY 'flag ' FLAG.
           MOVE 'NOWHERE' TO PNAME.
           CALL PNAME USING N
               ON OVERFLOW DISPLAY 'no such program'
           END-CALL.
      *> a DISPLAY inside the ON EXCEPTION branch would take the NOT
      *> clause for itself under GnuCOBOL (DISPLAY ... ON EXCEPTION is
      *> 2002); MOVEs keep the binding unambiguous
           CALL PNAME USING N
               ON EXCEPTION MOVE 'E' TO FLAG
               NOT ON EXCEPTION MOVE 'U' TO FLAG
           END-CALL.
           DISPLAY 'flag ' FLAG.
           MOVE 'TWICE' TO PNAME.
           CALL PNAME USING N.
           CANCEL PNAME.
           CALL PNAME USING N.
           DISPLAY 'after cancel: ' N.
           MOVE 'NOWHERE' TO PNAME.
           CALL 'NOWHERE' ON EXCEPTION DISPLAY 'literal: no such program'.
           CALL 'TWICE' USING N ON EXCEPTION DISPLAY 'unexpected'.
           DISPLAY 'end ' N.
           CANCEL PNAME.
           STOP RUN.
       END PROGRAM DYNCALL.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TWICE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CALLS        PIC 9 VALUE 0.
       LINKAGE SECTION.
       01  V            PIC 9(4).
       PROCEDURE DIVISION USING V.
           MULTIPLY 2 BY V.
           ADD 1 TO CALLS.
           DISPLAY 'twice: call ' CALLS.
           GOBACK.
       END PROGRAM TWICE.
