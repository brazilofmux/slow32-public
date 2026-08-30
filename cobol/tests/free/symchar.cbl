       IDENTIFICATION DIVISION.
       PROGRAM-ID. SYMCHAR.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           SYMBOLIC CHARACTERS TAB STAR ARE 10 43
           SYMBOLIC DOT IS 47.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  A            PIC X(3) VALUE STAR.
       01  B            PIC X(4) VALUE ALL DOT.
       01  C            PIC X(5).
       01  N            PIC 9(3) VALUE 0.
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY '[' A '][' B ']'.
           MOVE STAR TO C.
           DISPLAY '[' C ']'.
           MOVE ALL DOT TO C.
           DISPLAY '[' C ']'.
           IF C (2:1) = DOT DISPLAY 'dot compares'.
           MOVE 'a' TO C.
           STRING 'x' TAB 'y' DELIMITED BY SIZE INTO C.
           INSPECT C TALLYING N FOR ALL TAB.
           DISPLAY N.
           STOP RUN.
