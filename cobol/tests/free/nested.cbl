       IDENTIFICATION DIVISION.
       PROGRAM-ID. OUTER.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  SHARED IS GLOBAL.
           05  TALLY-G      PIC 9(3) VALUE 0.
           05  NOTE-G       PIC X(10) VALUE 'untouched'.
           88  NOTE-SET     VALUE 'set'.
       01  PRIVATE-W        PIC X(5) VALUE 'outer'.
       PROCEDURE DIVISION.
       MAIN SECTION.
       START-UP.
           CALL 'INNER-A'.
           DISPLAY 'tally ' TALLY-G ' note ' NOTE-G.
           IF NOTE-SET DISPLAY 'condition on a global item holds'.
           CALL 'INNER-A'.
           CALL 'INNER-A'.
           DISPLAY 'tally ' TALLY-G.
           CALL 'FRESH'.
           CALL 'FRESH'.
           DISPLAY 'private ' PRIVATE-W.
           STOP RUN.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. INNER-A.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  PRIVATE-W        PIC X(5) VALUE 'inner'.
       01  VISITS           PIC 9 VALUE 0.
       PROCEDURE DIVISION.
       A-MAIN.
           ADD 1 TO VISITS.
           ADD 1 TO TALLY-G.
           MOVE 'set' TO NOTE-G.
           DISPLAY 'inner-a visit ' VISITS ' sees ' PRIVATE-W.
           IF VISITS = 1
               CALL 'INNER-B'.
           EXIT PROGRAM.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. INNER-B.
       PROCEDURE DIVISION.
       B-MAIN.
           ADD 10 TO TALLY-G.
           DISPLAY 'inner-b, two levels down, tally ' TALLY-G.
           EXIT PROGRAM.
       END PROGRAM INNER-B.
       END PROGRAM INNER-A.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. FRESH IS INITIAL PROGRAM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  COUNT-W          PIC 9 VALUE 0.
       PROCEDURE DIVISION.
       F-MAIN.
           ADD 1 TO COUNT-W.
           DISPLAY 'fresh count ' COUNT-W.
           EXIT PROGRAM.
       END PROGRAM FRESH.
       END PROGRAM OUTER.
