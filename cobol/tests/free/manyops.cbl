       IDENTIFICATION DIVISION.
       PROGRAM-ID. MANYOPS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ONES.
           05  D OCCURS 21 TIMES PIC 9 VALUE 1.
       01  T            PIC S9(3) VALUE 0.
       PROCEDURE DIVISION.
       MAIN.
           ADD D (1) D (2) D (3) D (4) D (5) D (6) D (7) D (8) D (9)
               D (10) D (11) D (12) D (13) D (14) D (15) D (16) D (17)
               D (18) D (19) D (20) D (21) TO T.
           DISPLAY T.
           SUBTRACT D (1) D (2) D (3) D (4) D (5) D (6) D (7) D (8) D (9)
               D (10) D (11) D (12) D (13) D (14) D (15) D (16) D (17)
               D (18) D (19) D (20) D (21) FROM T.
           DISPLAY T.
           ADD 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
               24 25 26 27 28 29 30 TO T GIVING T.
           DISPLAY T.
           STOP RUN.
