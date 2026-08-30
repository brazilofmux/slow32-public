       IDENTIFICATION DIVISION.
       PROGRAM-ID. VARY4.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  I            PIC 9 VALUE 0.
       01  J            PIC 9 VALUE 0.
       01  K            PIC 9 VALUE 0.
       01  L            PIC 9 VALUE 0.
       01  N            PIC 9(3) VALUE 0.
       01  LAST-SEEN    PIC X(4).
       PROCEDURE DIVISION.
       MAIN.
           PERFORM COUNT-IT
               VARYING I FROM 1 BY 1 UNTIL I > 2
               AFTER J FROM 1 BY 1 UNTIL J > 3
               AFTER K FROM 1 BY 1 UNTIL K > 2
               AFTER L FROM 1 BY 2 UNTIL L > 3.
           DISPLAY N ' ' LAST-SEEN ' ' I ' ' J ' ' K ' ' L.
           MOVE 0 TO N.
           PERFORM WITH TEST AFTER
               VARYING I FROM 1 BY 1 UNTIL I >= 2
               AFTER J FROM 5 BY -1 UNTIL J < 4
               AFTER K FROM 1 BY 1 UNTIL K >= 1
               AFTER L FROM 2 BY 2 UNTIL L >= 4
               ADD 1 TO N
               MOVE 'x' TO LAST-SEEN
           END-PERFORM.
           DISPLAY N ' ' I ' ' J ' ' K ' ' L.
           STOP RUN.
       COUNT-IT.
           ADD 1 TO N.
           STRING I J K L DELIMITED BY SIZE INTO LAST-SEEN.
