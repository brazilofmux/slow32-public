      *> A non-integer numeric DISPLAY or COMP item sent to an alphanumeric
      *> receiver: the digits as stored, the sign and the point
      *> unrepresented.  Stage 53 settled this on the NIST cases against the
      *> text (086ee808, the user's ruling of 2026-08-31).  GnuCOBOL calls it
      *> "invalid MOVE" and refuses it in every dialect, so there is
      *> no oracle for this one; the .expected file is the cases' answer.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NUMALNUM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  N1           PIC 9(3)V99 VALUE 123.45.
       01  N2           PIC S9(2)V9 VALUE -7.5.
       01  N3           PIC 9V9(3) COMP VALUE 0.125.
       01  A            PIC X(8).
       01  B            PIC X(3).
       PROCEDURE DIVISION.
       MAIN.
           MOVE N1 TO A.
           DISPLAY '[' A ']'.
           MOVE N2 TO A.
           DISPLAY '[' A ']'.
           MOVE N3 TO A.
           DISPLAY '[' A ']'.
           MOVE N1 TO B.
           DISPLAY '[' B ']'.
           STOP RUN.
