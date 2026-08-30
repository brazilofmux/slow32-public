       IDENTIFICATION DIVISION.
       PROGRAM-ID. INSPINIT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  S            PIC X(24).
       01  N1           PIC 99.
       01  N2           PIC 99.
       01  N3           PIC 99.
       01  D            PIC X VALUE 'X'.
       PROCEDURE DIVISION.
       MAIN.
           MOVE 'AABAXBABAYAB ABA' TO S.
           MOVE 0 TO N1 N2 N3.
           INSPECT S TALLYING N1 FOR ALL 'A' BEFORE INITIAL 'Y'
                              N2 FOR ALL 'A' AFTER INITIAL 'X'
                              N3 FOR CHARACTERS AFTER 'X' BEFORE 'Y'.
           DISPLAY N1 ' ' N2 ' ' N3.
           MOVE 0 TO N1 N2 N3.
           INSPECT S TALLYING N1 FOR LEADING 'A' AFTER INITIAL D
                              N2 FOR ALL 'Q' BEFORE INITIAL 'Z'
                              N3 FOR CHARACTERS AFTER 'ZZ'.
           DISPLAY N1 ' ' N2 ' ' N3.
           INSPECT S REPLACING ALL 'A' BY '-' BEFORE INITIAL 'X'.
           DISPLAY '[' S ']'.
           INSPECT S REPLACING FIRST 'B' BY '+' AFTER 'Y'
                               CHARACTERS BY '.' AFTER ' ' BEFORE 'Z'.
           DISPLAY '[' S ']'.
           MOVE 'AHAH YES AH AH' TO S.
           INSPECT S REPLACING LEADING 'AH' BY 'OH' BEFORE INITIAL ' AH YES'.
           DISPLAY '[' S ']'.
           MOVE 'hello world, hello moon' TO S.
           INSPECT S CONVERTING 'elo' TO 'ELO' AFTER INITIAL ','.
           DISPLAY '[' S ']'.
           MOVE 0 TO N1 N2.
           INSPECT S TALLYING N1 FOR ALL 'l' N2 FOR CHARACTERS BEFORE ','
                     REPLACING ALL 'l' BY 'L' AFTER 'w'.
           DISPLAY N1 ' ' N2 ' [' S ']'.
           STOP RUN.
