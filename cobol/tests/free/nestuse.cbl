       IDENTIFICATION DIVISION.
       PROGRAM-ID. OUTER.
      *> no oracle: GnuCOBOL 4.0-early-dev never returns from a containing
      *> program's USE GLOBAL procedure invoked for a contained program's
      *> I/O (it hangs after the procedure runs); the NIST IC233A and IC234A
      *> programs, which GnuCOBOL does pass, cover the same rule and match
      *> ours (tests/ccvs-run.sh IC).
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GFILE ASSIGN TO 'nestuse-empty.txt'
               ORGANIZATION LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  GFILE GLOBAL.
       01  GREC             PIC X(20).
       WORKING-STORAGE SECTION.
       01  TALLY-G          PIC 9(3) VALUE 0 GLOBAL.
       PROCEDURE DIVISION.
       DECLARATIVES.
       ERR-SEC SECTION.
           USE GLOBAL AFTER STANDARD ERROR PROCEDURE ON INPUT.
       ERR-PARA.
           ADD 100 TO TALLY-G.
           DISPLAY 'outer USE GLOBAL ran'.
       END DECLARATIVES.
       MAIN SECTION.
       START-UP.
           OPEN OUTPUT GFILE.
           CLOSE GFILE.
           CALL 'INNER-A'.
           DISPLAY 'tally ' TALLY-G.
           STOP RUN.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. INNER-A.
       PROCEDURE DIVISION.
       DECLARATIVES.
       OWN-SEC SECTION.
           USE AFTER STANDARD ERROR PROCEDURE ON OUTPUT.
       OWN-PARA.
           ADD 10 TO TALLY-G.
           DISPLAY 'inner-a own USE ran (output only: not this one)'.
       END DECLARATIVES.
       A-MAIN.
           CALL 'INNER-B'.
           EXIT PROGRAM.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. INNER-B.
       PROCEDURE DIVISION.
       B-MAIN.
      *> the READ at end has no AT END phrase: the applicable procedure is
      *> found outward -- INNER-A's is for OUTPUT, OUTER's GLOBAL one for INPUT
           OPEN INPUT GFILE.
           READ GFILE.
           CLOSE GFILE.
           ADD 1 TO TALLY-G.
           EXIT PROGRAM.
       END PROGRAM INNER-B.
       END PROGRAM INNER-A.
       END PROGRAM OUTER.
