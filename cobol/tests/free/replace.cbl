       IDENTIFICATION DIVISION.
       PROGRAM-ID. REPLACE1.
      *> REPLACE: pseudo-text substitution over the source that follows,
      *> until REPLACE OFF or the next REPLACE.  A '=' inside pseudo-text
      *> (== = ==) is a text word, not a delimiter.
       REPLACE ==PICTURE== BY ==PIC==
               ==WIDE-NAME== BY ==WN==.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WN               PICTURE X(5) VALUE 'hello'.
       01  N                PICTURE 9(3) VALUE 7.
       01  D1               PIC X.
       01  D2               PIC X.
       REPLACE ==AO== BY ==TO== ==IE== BY ==IF== == = == BY ==EQUAL==
               ==DD1== BY ==D1==.
       PROCEDURE DIVISION.
       MAIN.
           MOVE 'a' AO DD1.
           MOVE 'b' AO D2.
           IE N = 7 DISPLAY 'seven ' WN D1 D2
              ELSE DISPLAY 'not seven'.
           REPLACE OFF.
           IF N = 7 DISPLAY 'plain again' END-IF.
           STOP RUN.
