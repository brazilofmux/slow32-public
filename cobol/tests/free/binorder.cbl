      *> COMP and BINARY are big-endian, COMP-5 native (docs/usage.md;
      *> default dialect: COMP-5): a record's bytes as written, then read back.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. binorder.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT f ASSIGN TO "binorder.dat"
               ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD f.
       01 rec.
          05 r-h   PIC S9(4) COMP.
          05 r-w   PIC 9(9) COMP.
          05 r-d   PIC S9(18) BINARY.
          05 r-n   PIC S9(4) COMP-5.
          05 r-v   PIC 9(3) COMP.
       WORKING-STORAGE SECTION.
       01 raw      PIC X(24).
       01 i        PIC 99.
       01 hx       PIC X(16) VALUE "0123456789ABCDEF".
       01 line-out PIC X(72).
       01 k        PIC 99.
       01 b        PIC 999.
       01 w-init   PIC 9(9) COMP VALUE 305419896.
       01 w-init-x REDEFINES w-init PIC X(4).
       01 tbl.
          05 t-e   PIC X(3) OCCURS 5.
       01 ix       PIC S9(4) COMP.
       01 tot      PIC S9(9) COMP VALUE 0.
       01 shown    PIC -9(18).
       PROCEDURE DIVISION.
           MOVE -2 TO r-h
           MOVE w-init TO r-w
           MOVE -1234567890123 TO r-d
           MOVE 258 TO r-n
           MOVE 0 TO r-v ADD 513 TO r-v
           OPEN OUTPUT f WRITE rec CLOSE f
           OPEN INPUT f READ f INTO raw CLOSE f
           PERFORM show-bytes
           MOVE w-init-x TO raw
           MOVE SPACES TO line-out
           PERFORM VARYING i FROM 1 BY 1 UNTIL i > 4
               COMPUTE b = FUNCTION ORD(raw(i:1)) - 1
               COMPUTE k = (i - 1) * 3 + 1
               MOVE hx(b / 16 + 1:1) TO line-out(k:1)
               MOVE hx(FUNCTION MOD(b, 16) + 1:1) TO line-out(k + 1:1)
           END-PERFORM
           DISPLAY "VALUE " line-out
           MOVE r-d TO shown DISPLAY "r-d " shown
           ADD r-h r-w r-n TO tot DISPLAY "tot " tot
           MOVE "aaabbbcccdddeee" TO tbl
           PERFORM VARYING ix FROM 5 BY -1 UNTIL ix < 1
               DISPLAY t-e(ix) WITH NO ADVANCING
           END-PERFORM
           DISPLAY SPACE
           COMPUTE r-h = r-h * 1000 - 7 DISPLAY "r-h " r-h
           MOVE r-h TO raw(1:2)
           STOP RUN.
       show-bytes.
           MOVE SPACES TO line-out
           PERFORM VARYING i FROM 1 BY 1 UNTIL i > 18
               COMPUTE b = FUNCTION ORD(raw(i:1)) - 1
               COMPUTE k = (i - 1) * 3 + 1
               MOVE hx(b / 16 + 1:1) TO line-out(k:1)
               MOVE hx(FUNCTION MOD(b, 16) + 1:1) TO line-out(k + 1:1)
           END-PERFORM
           DISPLAY "rec " line-out.
