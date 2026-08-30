       IDENTIFICATION DIVISION.
       PROGRAM-ID. INITREP.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  REC.
           05  AN       PIC X(4) VALUE 'abcd'.
           05  AL       PIC A(3) VALUE 'xyz'.
           05  NU       PIC 9(3)V9 VALUE 5.5.
           05  NE       PIC ZZ9.9 VALUE '  0.0'.
           05  AE       PIC XXBXX VALUE 'ab cd'.
           05  FILLER   PIC X(2) VALUE 'ff'.
           05  TBL OCCURS 3 TIMES.
               10  TN   PIC 99 VALUE 7.
               10  TX   PIC XX VALUE 'tt'.
       01  N1234        PIC 9(4) VALUE 1234.
       PROCEDURE DIVISION.
       MAIN.
           INITIALIZE REC.
           DISPLAY '[' REC ']'.
           MOVE 'ffff' TO AN.
           INITIALIZE REC REPLACING ALPHANUMERIC DATA BY '**'
                                    NUMERIC BY N1234
                                    ALPHABETIC BY 'Q'
                                    NUMERIC-EDITED DATA BY 12.5
                                    ALPHANUMERIC-EDITED BY 'ZZZZ'.
           DISPLAY '[' REC ']'.
           INITIALIZE TBL (2) REPLACING NUMERIC BY 42.
           DISPLAY '[' REC ']'.
           STOP RUN.
