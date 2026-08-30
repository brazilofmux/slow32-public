       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUBQUAL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  T.
           05  E OCCURS 4 TIMES PIC X.
       01  A1.
           05  A2.
               10  A3.
                   15  A4.
                       20  A5.
                           25  A6.
                               30  A7.
                                   35  A8.
                                       40  A9.
                                           45  A10.
                                               49  IX PIC 9 VALUE 3.
       01  B1.
           05  B2.
               10  IX PIC 9 VALUE 1.
       PROCEDURE DIVISION.
       MAIN.
           MOVE 'wxyz' TO T.
           DISPLAY E (IX OF A10 OF A9 OF A8 OF A7 OF A6 OF A5 OF A4 OF A3
                      OF A2 OF A1)
                   E (IX IN B2 IN B1)
                   E (IX OF A10 IN A9 OF A8 IN A7 OF A6 IN A5 OF A4 IN A3
                      OF A2 IN A1 + 1).
           STOP RUN.
