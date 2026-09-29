      * -warn-extensions under -std=85: a program within X3.23-1985 --
      * fixed form, PACKED-DECIMAL and BINARY, EXIT PROGRAM, plain STOP
      * RUN, near misses for every class E point -- draws no warning.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXTCLEAN.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  PK        PIC S9(5) PACKED-DECIMAL.
       01  BN        PIC S9(4) BINARY.
       01  MY-ITEM   PIC X VALUE "X".
       01  POS-LINE  PIC X(10) VALUE "LINE".
       PROCEDURE DIVISION.
       P1.
           MOVE 1 TO PK BN
           DISPLAY POS-LINE
           STOP RUN.
