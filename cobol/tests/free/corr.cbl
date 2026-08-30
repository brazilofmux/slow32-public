       IDENTIFICATION DIVISION.
       PROGRAM-ID. CORR1.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  SRC.
           05  NAME         PIC X(6) VALUE 'source'.
           05  QTY          PIC 9(3) VALUE 7.
           05  FILLER       PIC X(2) VALUE 'ff'.
           05  INNER.
               10  AMT      PIC 9(4)V99 VALUE 12.34.
               10  ONLY-SRC PIC X VALUE 's'.
           05  TBL OCCURS 2 TIMES.
               10  T-ITEM   PIC 9 VALUE 9.
           05  RATE         PIC 9V9 VALUE 1.5.
       01  DST.
           05  QTY          PIC 9(3) VALUE 100.
           05  FILLER       PIC X(2) VALUE 'gg'.
           05  INNER.
               10  AMT      PIC 9(4)V99 VALUE 1.00.
               10  ONLY-DST PIC X VALUE 'd'.
           05  NAME         PIC X(8) VALUE 'dest'.
           05  RATE         PIC 9 VALUE 3.
           05  TBL OCCURS 2 TIMES.
               10  T-ITEM   PIC 9 VALUE 0.
       01  BIG.
           05  N            PIC 9(2) VALUE 99.
           05  M            PIC 9(2) VALUE 5.
       01  SMALL.
           05  N            PIC 9 VALUE 1.
           05  M            PIC 9(3) VALUE 10.
       01  FLAG             PIC X VALUE '-'.
       PROCEDURE DIVISION.
       MAIN.
           ADD CORRESPONDING SRC TO DST ROUNDED.
           DISPLAY 'add:  ' QTY OF DST ' ' AMT OF DST ' ' RATE OF DST
                   ' ' T-ITEM OF DST (1) ' [' NAME OF DST ']'.
           SUBTRACT CORR SRC FROM DST.
           DISPLAY 'sub:  ' QTY OF DST ' ' AMT OF DST ' ' RATE OF DST.
           MOVE CORRESPONDING SRC TO DST.
           DISPLAY 'move: ' QTY OF DST ' ' AMT OF DST ' ' RATE OF DST
                   ' ' ONLY-DST ' [' NAME OF DST '] ' T-ITEM OF DST (2).
           ADD CORR BIG TO SMALL
               ON SIZE ERROR MOVE 'E' TO FLAG.
           DISPLAY 'size: ' N OF SMALL ' ' M OF SMALL ' ' FLAG.
           STOP RUN.
