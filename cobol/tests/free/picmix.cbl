       IDENTIFICATION DIVISION.
       PROGRAM-ID. PICMIX.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  N1       PICTURE 99; VALUE 8.
       77  N2       PIC S999, COMPUTATIONAL, VALUE -5.
       01  MIX1     PIC 9A9 VALUE ZERO.
       01  MIX2     PIC AB9.
       01  MIX3     PICTURE XBA09.
       01  MIX4     PIC A/AA.
       01  ZP       PICTURE ZZZPP VALUE ZERO.
       01  ZP2      PICTURE 9(3)PP.
       01  V        PIC 9(5) VALUE 12300.
       01  BACK     PIC 9(5).
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY N1 ' ' N2.
           DISPLAY '[' MIX1 ']'.
           MOVE 'AB' TO MIX2.
           MOVE 'XYZ' TO MIX3.
           MOVE 'PQR' TO MIX4.
           DISPLAY '[' MIX2 '][' MIX3 '][' MIX4 ']'.
           MOVE V TO ZP.
           MOVE V TO ZP2.
           DISPLAY '[' ZP '][' ZP2 ']'.
           MOVE 45678 TO ZP.
           MOVE ZP TO BACK.
           DISPLAY '[' ZP '] ' BACK.
           MOVE 7 TO ZP.
           DISPLAY '[' ZP ']'.
           STOP RUN.
