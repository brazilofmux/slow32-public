       IDENTIFICATION DIVISION.
       PROGRAM-ID. INSPORD.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 S1 PIC X(11).
       01 S2 PIC X(13).
       01 S3 PIC X(14).
       01 K PIC 9(4).
       PROCEDURE DIVISION.
       P0.
           MOVE ',BX1XAB' TO S1.
           INSPECT S1 REPLACING ALL 'X' BY '1' ALL '1X' BY '1X'
               ALL '1' BY 'A' AFTER INITIAL '1'.
           DISPLAY '1 [' S1 ']'.
           MOVE ' XABB1A1X1A1' TO S2.
           INSPECT S2 REPLACING FIRST 'B' BY ','
               LEADING 'B' BY ' ' AFTER INITIAL 'A'.
           DISPLAY '2 [' S2 ']'.
           MOVE '1AY,XAY' TO S3.
           MOVE 0 TO K.
           INSPECT S3 TALLYING K FOR CHARACTERS BEFORE INITIAL 'Y'
               REPLACING ALL 'X' BY 'A' FIRST ',X' BY 'XA'
               ALL 'A1' BY '1Y'.
           DISPLAY '3 [' S3 '] ' K.
           STOP RUN.
