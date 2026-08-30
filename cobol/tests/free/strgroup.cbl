       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRGROUP.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  G.
           05  G1       PIC X(3) VALUE 'xxx'.
           05  G2       PIC 9(3) VALUE 999.
           05  G3       PIC X(2) VALUE 'zz'.
       01  P            PIC 99 VALUE 2.
       PROCEDURE DIVISION.
       MAIN.
           STRING 'ABCDEF' DELIMITED BY SIZE INTO G WITH POINTER P.
           DISPLAY '[' G '] ' P.
           STRING 'Q' DELIMITED BY SIZE INTO G WITH POINTER P
               ON OVERFLOW DISPLAY 'overflow'.
           DISPLAY '[' G '] ' P.
           STOP RUN.
