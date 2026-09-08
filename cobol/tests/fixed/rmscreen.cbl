       IDENTIFICATION DIVISION.
       PROGRAM-ID. RMSCREEN.
      * RM/COBOL's positioned DISPLAY/ACCEPT (GitHub #32) and the 1983
      * AT rrcc spelling (#33), as the Open Systems suite paints every
      * screen: LINE/POSITION literal and computed, ERASE EOS/EOL/SCREEN,
      * SIZE, HIGH, PROMPT, UPDATE, NO BEEP, and a plain DISPLAY once
      * the screen is in use (the next line, column 1), and LINE 0
      * POSITION 0, the cursor's own position. Screens need a
      * tty: no oracle. The keys come from rmscreen.keys; the ANSI
      * stream is the expected output.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  L        PIC 99 COMP VALUE 5.
       77  P        PIC 99 COMP VALUE 7.
       77  AMT      PIC Z,ZZ9.99.
       77  NAME-IN  PIC X(6) VALUE "OLD".
       77  CODE-IN  PIC X(4) VALUE SPACES.
       77  RRCC     PIC 9(4) VALUE 1203.
       PROCEDURE DIVISION.
       MAIN.
           MOVE 1234.5 TO AMT.
           DISPLAY "TRIAL BALANCE" LINE 1, POSITION 5, ERASE EOS.
           DISPLAY "PICK BY" LINE 3, POSITION 3.
           DISPLAY "ACCOUNT NUMBERS  FROM".
           DISPLAY AMT LINE L POSITION P, HIGH.
           DISPLAY "X" LINE 22, SIZE 3, ERASE EOL.
           DISPLAY "AT" AT 1010 WITH ERASE EOS.
           DISPLAY "RR" AT RRCC.
           ACCEPT CODE-IN LINE 6, POSITION 3, PROMPT, NO BEEP.
           ACCEPT NAME-IN LINE 7, POSITION 3, PROMPT, UPDATE.
           DISPLAY CODE-IN LINE 9 POSITION 1 SIZE 3.
           DISPLAY "GOT " NAME-IN.
           DISPLAY "PRINT " LINE 15 POSITION 1.
           DISPLAY "PURCHASES" LINE 0 POSITION 0.
           DISPLAY " JOURNAL" LINE 0 POSITION 0.
           DISPLAY SPACE ERASE SCREEN.
           STOP RUN.
