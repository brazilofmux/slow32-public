       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLASSLIT.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CLASS HEXDIGIT IS "0123456789" "ABCDEF" "abcdef"
           CLASS VOWELS IS "AEIOU" 'a' THROUGH 'e'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  S            PIC X(6).
       PROCEDURE DIVISION.
       MAIN.
           MOVE '1aF09b' TO S.
           IF S IS HEXDIGIT DISPLAY 'hex' ELSE DISPLAY 'not hex'.
           MOVE '1aG09b' TO S.
           IF S IS HEXDIGIT DISPLAY 'hex' ELSE DISPLAY 'not hex'.
           MOVE 'AEIeba' TO S.
           IF S IS VOWELS DISPLAY 'vowels' ELSE DISPLAY 'not vowels'.
           MOVE 'AEIfba' TO S.
           IF S IS NOT VOWELS DISPLAY 'not vowels' ELSE DISPLAY 'vowels'.
           STOP RUN.
