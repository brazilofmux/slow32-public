       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALTER3.
      * ALTER nested inside IF/ELSE (GitHub #36): the prescan that
      * gathers ALTER targets must see it there too -- GENSRT19's
      * polyphase merge selects its output file that way.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  N PIC 9 VALUE 2.
       PROCEDURE DIVISION.
       MAIN.
           IF N = 1
               ALTER WRITE-FILE TO PROCEED TO WRITE-A
           ELSE ALTER WRITE-FILE TO PROCEED TO WRITE-B.
           PERFORM WRITE-FILE THRU WRITE-EXIT.
           MOVE 1 TO N.
           IF N = 1
               ALTER WRITE-FILE TO PROCEED TO WRITE-A
           ELSE ALTER WRITE-FILE TO PROCEED TO WRITE-B.
           PERFORM WRITE-FILE THRU WRITE-EXIT.
           STOP RUN.
       WRITE-FILE.
           GO TO WRITE-A.
       WRITE-A.
           DISPLAY "A".
           GO TO WRITE-EXIT.
       WRITE-B.
           DISPLAY "B".
       WRITE-EXIT.
           EXIT.
