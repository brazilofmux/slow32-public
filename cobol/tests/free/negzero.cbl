IDENTIFICATION DIVISION.
PROGRAM-ID. NEGZERO.
*> A negative value whose digits its receiver cuts to nothing: -10000
*> into PIC S9(4).  The sign of the numeric value is represented in a
*> signed receiver (2023 14.9.25.4, MOVE; X3.23-1985 the same), so the
*> item holds a zero with a negative sign -- "000p" -- and zero being one
*> value whatever its sign (8.8.4.2.2), it compares equal to zero and is
*> displayed "+0000".  Two things were out of step (cobol ISSUES-122):
*> DISPLAY showed a zoned item's stored sign, "-0000", where a packed or
*> binary one showed "+"; and the wide store dropped the sign the 64-bit
*> store kept, so one statement gave "000p" or "0000" by the size of its
*> operands.  GnuCOBOL agrees with every line, the bytes too.
DATA DIVISION.
WORKING-STORAGE SECTION.
01  A   PIC S9(4) VALUE -100.
01  B   PIC S9(4) VALUE 100.
01  R   PIC S9(4).
01  RX  REDEFINES R PIC X(4).
01  RL  PIC S9(4) SIGN LEADING SEPARATE.
01  P   PIC S9(4) PACKED-DECIMAL.
01  BN  PIC S9(4) BINARY.
01  W   PIC S9(18) VALUE -99999.
01  X   PIC S9(18) VALUE 70000000000.
PROCEDURE DIVISION.
MAIN.
    COMPUTE R = A * B                DISPLAY "compute: " R " bytes [" RX "]"
    MULTIPLY A BY B GIVING R         DISPLAY "multiply giving: " R
    MOVE -10000 TO R                 DISPLAY "move: " R " bytes [" RX "]"
    MOVE -20000 TO RL                DISPLAY "sign separate: " RL
    COMPUTE P = A * B                DISPLAY "packed: " P
    COMPUTE BN = A * B               DISPLAY "binary: " BN
    COMPUTE R = W * X                DISPLAY "a product past 18 digits: " R
    SUBTRACT 10000 FROM 0 GIVING R   DISPLAY "subtract giving: " R
    MOVE -12345 TO R                 DISPLAY "a negative that keeps digits: " R
    IF R < 0 DISPLAY "... is negative" END-IF
    STOP RUN.
