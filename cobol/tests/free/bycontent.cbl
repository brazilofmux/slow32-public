       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONTENT1.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  A            PIC X(6) VALUE 'caller'.
       01  N            PIC 9(4) VALUE 10.
       01  R            PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
       MAIN.
           CALL 'BUMP' USING BY CONTENT A, N, BY REFERENCE R.
           DISPLAY 'after content: [' A '] ' N ' r=' R.
           CALL 'BUMP' USING BY REFERENCE A, N, R.
           DISPLAY 'after reference: [' A '] ' N ' r=' R.
           CALL 'BUMP' USING BY CONTENT 'litera', N, BY REFERENCE R.
           DISPLAY 'literal and item by content: r=' R ' n=' N.
           STOP RUN.
       END PROGRAM CONTENT1.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUMP.
       DATA DIVISION.
       LINKAGE SECTION.
       01  X            PIC X(6).
       01  V            PIC 9(4).
       01  OUT-V        PIC 9(4).
       PROCEDURE DIVISION USING X V OUT-V.
       B.
           DISPLAY 'bump sees [' X '] ' V.
           MOVE 'callee' TO X.
           ADD 1 TO V.
           MOVE V TO OUT-V.
           EXIT PROGRAM.
       END PROGRAM BUMP.
