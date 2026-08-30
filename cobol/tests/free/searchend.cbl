       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEARCHEND.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TBL.
           05  ROW OCCURS 5 TIMES ASCENDING KEY IS K1 K2 INDEXED BY IX.
               10  K1   PIC 99.
               10  K2   PIC 99.
       01  N            PIC 9.
       PROCEDURE DIVISION.
       MAIN.
           MOVE '0101010201030203' TO TBL.
           MOVE '0301' TO ROW (5).
           SEARCH ALL ROW END DISPLAY 'not found'
               WHEN K1 (IX) = 02 AND K2 (IX) = 03
                    SET N TO IX DISPLAY 'found at ' N.
           SEARCH ALL ROW END DISPLAY 'none'
               WHEN K1 (IX) = 09 AND K2 (IX) = 09
                    DISPLAY 'wrong'.
           SET IX TO 1.
           SEARCH ROW END DISPLAY 'serial: none'
               WHEN K2 (IX) = 03 SET N TO IX DISPLAY 'serial found at ' N
           END-SEARCH.
           STOP RUN.
