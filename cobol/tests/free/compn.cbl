      *> COMP-4, COMP-X and COMP-6 (docs/usage.md; default dialect:
      *> the COMP-n usages are Micro Focus's): sizes, bytes and values.
      *> A COMP-X item holds what its bytes hold, as MF has it; GnuCOBOL
      *> truncates to the picture (.oracle-expected, docs/oracles.md).
       IDENTIFICATION DIVISION.
       PROGRAM-ID. compn.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 rec.
          05 c4    PIC S9(4) COMP-4 VALUE -300.
          05 x1    PIC X COMP-X VALUE 200.
          05 x2    PIC XX COMP-X VALUE 4660.
          05 x3    PIC 9(5) COMP-X VALUE 70000.
          05 x7    PIC 9(16) COMP-X VALUE 1234567890123456.
          05 p6    PIC 9(5) COMP-6 VALUE 12345.
          05 p6e   PIC 9(4) COMP-6 VALUE 987.
          05 p6s   PIC S9(3) COMP-6 VALUE -12.
       01 raw      PIC X(40).
       01 i        PIC 99.
       01 n        PIC 99.
       01 hx       PIC X(16) VALUE "0123456789ABCDEF".
       01 line-out PIC X(80).
       01 k        PIC 99.
       01 b        PIC 999.
       01 tot      PIC 9(18).
       01 cx       PIC 9(2) COMP-X VALUE 90.
       PROCEDURE DIVISION.
           DISPLAY "sizes " FUNCTION LENGTH(c4) " " FUNCTION LENGTH(x1)
               " " FUNCTION LENGTH(x2) " " FUNCTION LENGTH(x3)
               " " FUNCTION LENGTH(x7) " " FUNCTION LENGTH(p6)
               " " FUNCTION LENGTH(p6e) " " FUNCTION LENGTH(p6s)
           PERFORM show-rec
           DISPLAY c4 " " x1 " " x2 " " x3 " " x7 " " p6 " " p6e " " p6s
           ADD 55 TO x1
           ADD 1 TO x2 x3 p6 p6e
           SUBTRACT 1 FROM x7
           MULTIPLY 3 BY c4
           COMPUTE tot = x1 + x2 + x3 + x7 + p6 + p6e
           PERFORM show-rec
           DISPLAY c4 " " x1 " " x2 " " x3 " " x7 " " p6 " " p6e " " p6s
           DISPLAY "tot " tot
           MOVE 99999 TO p6 MOVE 0 TO p6e
           MOVE p6 TO x3
           PERFORM show-rec
           IF x3 = p6 AND p6e = ZERO AND x1 > 254
               DISPLAY "compare ok"
           END-IF
           ADD 50 TO cx DISPLAY "capacity " cx
           STOP RUN.
       show-rec.
           MOVE rec TO raw
           MOVE SPACES TO line-out
           COMPUTE n = FUNCTION LENGTH(rec)
           PERFORM VARYING i FROM 1 BY 1 UNTIL i > n
               COMPUTE b = FUNCTION ORD(raw(i:1)) - 1
               COMPUTE k = (i - 1) * 3 + 1
               MOVE hx(b / 16 + 1:1) TO line-out(k:1)
               MOVE hx(FUNCTION MOD(b, 16) + 1:1) TO line-out(k + 1:1)
           END-PERFORM
           DISPLAY line-out.
