       IDENTIFICATION DIVISION.
       PROGRAM-ID. MF-COMPX8.
      *> PIC X(8) COMP-X: eight bytes, unsigned, big-endian, holding to
      *> 2^64 - 1 (twenty digits) -- the field's capacity, as Micro
      *> Focus's COMP-X rules have it (docs/usage.md); the wide item
      *> BINARY-DOUBLE UNSIGNED in COMP-X's byte order.  ACAS's
      *> CBL-FILE-SIZE (cobol ISSUES-124).  The bytes are shown through
      *> a REDEFINES.  GnuCOBOL -std=mf gets the values past 2^63 wrong
      *> (docs/oracles.md).
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CX8          PIC X(8) COMP-X VALUE ZERO.
       01  CX8-BYTES REDEFINES CX8.
           05  CXB      PIC X OCCURS 8.
       01  D20          PIC 9(20).
       01  I            PIC 9.
       01  HEXD         PIC X(16).
       01  HX           PIC X(16) VALUE "0123456789ABCDEF".
       01  B            PIC 9(3).
       PROCEDURE DIVISION.
       MAIN.
           MOVE 1 TO CX8. PERFORM SHOW.
           MOVE 4294967296 TO CX8. PERFORM SHOW.
           COMPUTE CX8 = 4611686018427387904 * 2 + 5. PERFORM SHOW.
           COMPUTE CX8 = 4294967296 * 4294967295 + 4294967295.
           PERFORM SHOW.
           SUBTRACT 1 FROM CX8. PERFORM SHOW.
           COMPUTE CX8 = 1000000 * 1000000 + 7. PERFORM SHOW.
           ADD 1 TO CX8. PERFORM SHOW.
           IF CX8 > 999999999999 DISPLAY "GREATER" END-IF.
           STOP RUN.
       SHOW.
           MOVE CX8 TO D20.
           MOVE SPACES TO HEXD.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 8
               COMPUTE B = FUNCTION ORD(CXB(I)) - 1
               MOVE HX(B / 16 + 1:1) TO HEXD(I * 2 - 1:1)
               MOVE HX(FUNCTION MOD(B, 16) + 1:1) TO HEXD(I * 2:1)
           END-PERFORM.
           DISPLAY D20 " " HEXD.
