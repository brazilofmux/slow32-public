       IDENTIFICATION DIVISION.
       PROGRAM-ID. ODONEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  D            PIC 9 VALUE 3.
       01  GRP.
           05  HEAD     PIC X(2) VALUE 'h:'.
           05  SUB-GRP.
               10  TAB OCCURS 0 TO 5 TIMES DEPENDING ON D PIC X.
       01  DEEP.
           05  LVL1.
               10  LVL2.
                   15  ITEM OCCURS 1 TO 4 TIMES DEPENDING ON D PIC 9.
       01  RECV         PIC X(12) VALUE ALL '.'.
       PROCEDURE DIVISION.
       MAIN.
      *> the receiving side at full depth: a receiving group with an ODO
      *> table takes its maximum length by the 85 text (free/odomove is
      *> that divergence); this test is the sending length, table deep
           MOVE 4 TO D.
           MOVE 1234 TO LVL2.
           MOVE DEEP TO RECV.
           DISPLAY '[' RECV ']'.
           MOVE 3 TO D.
           MOVE 'abcde' TO SUB-GRP.
           MOVE ALL '.' TO RECV.
           MOVE GRP TO RECV.
           DISPLAY '[' RECV ']'.
           MOVE 1 TO D.
           MOVE ALL '.' TO RECV.
           MOVE GRP TO RECV.
           DISPLAY '[' RECV ']'.
           MOVE 2 TO D.
           MOVE ALL '.' TO RECV.
           MOVE DEEP TO RECV.
           DISPLAY '[' RECV ']'.
           STOP RUN.
