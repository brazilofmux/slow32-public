       IDENTIFICATION DIVISION.
       PROGRAM-ID. RENAMES1.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  REC.
           05  A1        PIC XX VALUE 'ab'.
           05  A2.
               10  A2A   PIC 9(3) VALUE 123.
               10  A2B   PIC X VALUE 'z'.
           05  A3        PIC X(4) VALUE 'wxyz'.
       66  R-ALL   RENAMES A1 THRU A3.
       66  R-MID   RENAMES A2.
       66  R-NUM   RENAMES A2A.
       66  R-TAIL  RENAMES A2B THRU A3.
       01  OTH.
           05  A1        PIC X(3) VALUE 'pqr'.
           05  A2        PIC X(2) VALUE 'st'.
       66  R-ALL   RENAMES A1 OF OTH THRU A2 OF OTH.
       01  N            PIC 9(4).
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY '[' R-ALL OF REC '][' R-MID '][' R-NUM '][' R-TAIL ']'.
           ADD 1 TO R-NUM.
           DISPLAY A2A ' ' R-NUM.
           MOVE 'QQ' TO R-TAIL.
           DISPLAY '[' REC ']'.
           MOVE 'hello' TO R-ALL OF OTH.
           DISPLAY '[' OTH ']'.
           MOVE R-NUM TO N.
           DISPLAY N.
           STOP RUN.
