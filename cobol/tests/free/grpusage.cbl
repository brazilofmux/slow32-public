       IDENTIFICATION DIVISION.
       PROGRAM-ID. GRPUSAGE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  BIN-GRP USAGE COMP.
           05  B1        PIC S9(4) VALUE -12.
           05  B2        PIC 9(8) VALUE 123456.
           05  B-SUB.
               10  B3    PIC S9(9) VALUE 7.
       01  IX-GRP USAGE IS INDEX.
           05  IX1.
           05  IX2.
       01  TBL.
           05  ENTRY-X OCCURS 5 TIMES INDEXED BY TI PIC X.
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY B1 ' ' B2 ' ' B3 ' len ' LENGTH OF BIN-GRP.
           ADD 1 TO B1 B2 B3.
           DISPLAY B1 ' ' B2 ' ' B3.
           SET TI TO 3.
           SET IX1 TO TI.
           SET TI UP BY 1.
           SET IX2 TO TI.
           SET TI TO IX1.
           MOVE 'abcde' TO TBL.
           DISPLAY ENTRY-X (TI) ' len ' LENGTH OF IX-GRP.
           IF IX1 < IX2 DISPLAY 'ix1 before ix2'.
           STOP RUN.
