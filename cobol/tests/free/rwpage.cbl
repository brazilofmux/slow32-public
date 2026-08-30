       IDENTIFICATION DIVISION.
       PROGRAM-ID. RWPAGE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRT ASSIGN TO 'rwpage.prn'
               ORGANIZATION LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  PRT REPORT IS REP.
       WORKING-STORAGE SECTION.
       01  N            PIC 99 VALUE 0.
       01  LC-SEEN      PIC 99.
       01  PC-SEEN      PIC 99.
       REPORT SECTION.
       RD  REP
           PAGE LIMIT 12 LINES
           HEADING 1
           FIRST DETAIL 3
           LAST DETAIL 8
           FOOTING 10.
       01  HEAD TYPE PAGE HEADING LINE 1.
           05  COLUMN 1  PIC X(11) VALUE 'report page'.
           05  COLUMN 13 PIC Z9    SOURCE PAGE-COUNTER.
       01  DET TYPE DETAIL LINE PLUS 1 COLUMN 3 PIC X(4) VALUE 'item'.
       01  DET2 TYPE DE.
           05  LINE PLUS 1 COLUMN 3 PIC X(5) VALUE 'entry'.
           05  COLUMN 9 PIC 99 SOURCE N.
           05  COLUMN 13 PIC X(2) VALUE 'lc'.
           05  COLUMN 16 PIC 99 SOURCE LINE-COUNTER.
       01  FOOT TYPE PAGE FOOTING LINE 11.
           05  COLUMN 1 PIC X(9) VALUE 'page foot'.
           05  COLUMN 11 PIC Z9 SOURCE PAGE-COUNTER.
       PROCEDURE DIVISION.
       MAIN.
           OPEN OUTPUT PRT.
           INITIATE REP.
           MOVE LINE-COUNTER TO LC-SEEN.
           MOVE PAGE-COUNTER TO PC-SEEN.
           DISPLAY 'after initiate: line ' LC-SEEN ' page ' PC-SEEN.
           PERFORM 9 TIMES
               ADD 1 TO N
               GENERATE DET2
               MOVE LINE-COUNTER TO LC-SEEN
               MOVE PAGE-COUNTER TO PC-SEEN
               DISPLAY 'n ' N ' on line ' LC-SEEN ' of page ' PC-SEEN
           END-PERFORM.
           GENERATE DET.
           MOVE LINE-COUNTER OF REP TO LC-SEEN.
           DISPLAY 'item on line ' LC-SEEN.
           TERMINATE REP.
           MOVE PAGE-COUNTER OF REP TO PC-SEEN.
           DISPLAY 'after terminate: page ' PC-SEEN.
           CLOSE PRT.
           STOP RUN.
