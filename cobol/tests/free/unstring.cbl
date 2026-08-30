       IDENTIFICATION DIVISION.
       PROGRAM-ID. UNSTR.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  SRC        PIC X(30) VALUE 'alpha,beta;;gamma  delta'.
       01  A          PIC X(6).
       01  B          PIC X(6).
       01  C          PIC X(6).
       01  D          PIC X(6).
       01  D1         PIC X(2).
       01  D2         PIC X(2).
       01  D3         PIC X(2).
       01  C1         PIC 99.
       01  C2         PIC 99.
       01  C3         PIC 99.
       01  P          PIC 99.
       01  T          PIC 99 VALUE 0.
       01  N          PIC 9(3).
       01  NUMS       PIC X(12) VALUE '12 345 6789'.
       01  NA         PIC 9(3).
       01  NB         PIC S9(4).
       01  NC         PIC 9(2).
       01  J          PIC X(5) JUSTIFIED RIGHT.
       PROCEDURE DIVISION.
       MAIN.
           UNSTRING SRC DELIMITED BY ',' OR ALL ';' OR ALL SPACE
               INTO A DELIMITER IN D1 COUNT IN C1
                    B DELIMITER IN D2 COUNT IN C2
                    C DELIMITER IN D3 COUNT IN C3
                    D
               TALLYING IN T.
           DISPLAY '[' A '][' B '][' C '][' D ']'.
           DISPLAY 'delims [' D1 '][' D2 '][' D3 '] counts ' C1 ' ' C2 ' ' C3 ' tally ' T.
           MOVE 7 TO P.
           UNSTRING SRC DELIMITED BY ';' INTO A, B WITH POINTER P
               ON OVERFLOW DISPLAY 'overflow, pointer ' P
               NOT ON OVERFLOW DISPLAY 'no overflow'.
           DISPLAY '[' A '][' B ']'.
           UNSTRING NUMS DELIMITED BY ALL ' ' INTO NA NB NC.
           DISPLAY NA ' ' NB ' ' NC.
           UNSTRING SRC DELIMITED BY ',' INTO J.
           DISPLAY '[' J ']'.
           MOVE 'abcdefghij' TO SRC.
           UNSTRING SRC INTO A B C.
           DISPLAY '[' A '][' B '][' C ']'.
           MOVE 99 TO P.
           UNSTRING SRC INTO A WITH POINTER P
               ON OVERFLOW DISPLAY 'bad pointer overflow'.
           STOP RUN.
