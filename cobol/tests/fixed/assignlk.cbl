       IDENTIFICATION DIVISION.
       PROGRAM-ID. ASSIGNLK.
      * ASSIGN TO a LINKAGE item: the subprogram's file name comes from
      * its caller, and a second CALL with a new name opens a new file.
      * The 2023 text forbids only an item of the file's own record
      * (12.4.5.2 rule 7); ACAS passes its file names this way.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FILE-NAMES.
           05  FN-OUT   PIC X(32) VALUE "ASGLK1.DAT".
       PROCEDURE DIVISION.
       MAIN.
           CALL "ASGLKSUB" USING FILE-NAMES.
           MOVE "ASGLK2.DAT" TO FN-OUT.
           CALL "ASGLKSUB" USING FILE-NAMES.
           STOP RUN.
       END PROGRAM ASSIGNLK.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ASGLKSUB.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUT-FILE ASSIGN TO LK-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS.
       DATA DIVISION.
       FILE SECTION.
       FD  OUT-FILE.
       01  OUT-REC      PIC X(20).
       WORKING-STORAGE SECTION.
       01  WS-FS        PIC XX.
       01  IN-REC       PIC X(20).
       LINKAGE SECTION.
       01  LK-NAMES.
           05  LK-NAME  PIC X(32).
       PROCEDURE DIVISION USING LK-NAMES.
       MAIN.
           OPEN OUTPUT OUT-FILE.
           MOVE LK-NAME TO OUT-REC.
           WRITE OUT-REC.
           CLOSE OUT-FILE.
           MOVE SPACES TO IN-REC.
           OPEN INPUT OUT-FILE.
           READ OUT-FILE INTO IN-REC.
           CLOSE OUT-FILE.
           DISPLAY "WROTE AND READ " IN-REC " STATUS " WS-FS.
           GOBACK.
       END PROGRAM ASGLKSUB.
