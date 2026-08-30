       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXTMAIN.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SHARED-FILE ASSIGN TO 'external-shared.dat'
               FILE STATUS IS MAIN-FS.
       DATA DIVISION.
       FILE SECTION.
       FD  SHARED-FILE IS EXTERNAL RECORD CONTAINS 12 CHARACTERS.
       01  SHARED-REC       PIC X(12).
       WORKING-STORAGE SECTION.
       01  EXT-BLOCK IS EXTERNAL.
           05  EXT-COUNT    PIC 9(3).
           05  EXT-NOTE     PIC X(8).
       01  MAIN-FS          PIC XX.
       01  SUB-FS           PIC XX.
       PROCEDURE DIVISION.
       MAIN.
           MOVE 5 TO EXT-COUNT.
           MOVE 'main' TO EXT-NOTE.
           CALL 'EXTSUB' USING SUB-FS.
           DISPLAY 'main sees ' EXT-COUNT ' [' EXT-NOTE ']'.
           OPEN OUTPUT SHARED-FILE.
           MOVE 'from-main' TO SHARED-REC.
           WRITE SHARED-REC.
           MOVE '<>' TO MAIN-FS.
           CALL 'EXTSUB' USING SUB-FS.
           DISPLAY 'sub wrote status [' SUB-FS '] main status [' MAIN-FS ']'.
           CLOSE SHARED-FILE.
           OPEN INPUT SHARED-FILE.
           READ SHARED-FILE.
           DISPLAY 'first: ' SHARED-REC.
           READ SHARED-FILE.
           DISPLAY 'second: ' SHARED-REC.
           READ SHARED-FILE
               AT END DISPLAY 'main at end, status ' MAIN-FS.
           CLOSE SHARED-FILE.
           STOP RUN.
       END PROGRAM EXTMAIN.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXTSUB.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SHARED-FILE ASSIGN TO 'external-shared.dat'
               FILE STATUS IS OWN-FS.
       DATA DIVISION.
       FILE SECTION.
       FD  SHARED-FILE IS EXTERNAL RECORD CONTAINS 12 CHARACTERS.
       01  SHARED-REC       PIC X(12).
       WORKING-STORAGE SECTION.
       01  EXT-BLOCK IS EXTERNAL.
           05  EXT-COUNT    PIC 9(3).
           05  EXT-NOTE     PIC X(8).
       01  VISITS           PIC 9 VALUE 0.
       LINKAGE SECTION.
       01  OWN-FS           PIC XX.
       PROCEDURE DIVISION USING OWN-FS.
       S-MAIN.
           ADD 1 TO VISITS.
           IF VISITS = 1
               DISPLAY 'sub sees ' EXT-COUNT ' [' EXT-NOTE ']'
               ADD 10 TO EXT-COUNT
               MOVE 'sub' TO EXT-NOTE
           ELSE
               MOVE 'from-sub' TO SHARED-REC
               WRITE SHARED-REC
           END-IF.
           EXIT PROGRAM.
       END PROGRAM EXTSUB.
