       IDENTIFICATION DIVISION.
       PROGRAM-ID. EDITINS.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 E1 PIC $0$$.99.
       01 E2 PIC +Z0ZZ9999/99.9.
       01 E3 PIC Z09,909999.999.
       01 E5 PIC Z/ZZZZZZ.ZZ.
       01 E4 PIC *0*9999999.999.
       01 E6 PIC ***/999999.9999.
       01 E7 PIC *999.9B99.
       01 E8 PIC **B909B9999.9999.
       01 E9 PIC /999.
       01 EA PIC 0999.
       01 EB PIC B999,999.
       01 SN PIC S9(8)V9(3) VALUE ZERO.
       01 EC PIC $9+.
       01 ED PIC 99CR.
       PROCEDURE DIVISION.
       P0.
           MOVE 0.42 TO E1. DISPLAY '1 [' E1 ']'.
           MOVE 0 TO E2. DISPLAY '2 [' E2 ']'.
           MOVE 6865.48 TO E3. DISPLAY '3 [' E3 ']'.
           MOVE 3387.1 TO E5. DISPLAY '5 [' E5 ']'.
           MOVE 444 TO E4. DISPLAY '4 [' E4 ']'.
           MOVE 27843.419 TO E6. DISPLAY '6 [' E6 ']'.
           MOVE 1472.3191 TO E7. DISPLAY '7 [' E7 ']'.
           MOVE 85805 TO E8. DISPLAY '8 [' E8 ']'.
           MOVE 12 TO E9. DISPLAY '9 [' E9 ']'.
           MOVE 12 TO EA. DISPLAY 'A [' EA ']'.
           MOVE 1234 TO EB. DISPLAY 'B [' EB ']'.
           MOVE -32745520.019 TO SN. MOVE SN TO EC.
           DISPLAY 'C [' EC ']'.
           MOVE -500 TO SN. MOVE SN TO ED.
           DISPLAY 'D [' ED ']'.
           STOP RUN.
