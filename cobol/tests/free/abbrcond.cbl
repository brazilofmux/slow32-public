       IDENTIFICATION DIVISION.
       PROGRAM-ID. ABBRCOND.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  A            PIC 99 VALUE 15.
       01  B            PIC 99 VALUE 10.
       01  C            PIC 99 VALUE 20.
       01  X            PIC X VALUE 'q'.
       01  R            PIC X(3).
       01  FLAG         PIC X VALUE 'y'.
           88  FLAG-ON  VALUE 'y'.
       PROCEDURE DIVISION.
       MAIN.
           IF A > B AND < C DISPLAY 'between' ELSE DISPLAY 'not between'.
           IF A > B AND NOT < C DISPLAY 'yes' ELSE DISPLAY 'no'.
           IF A = 1 OR 7 OR 15 DISPLAY 'one of them' ELSE DISPLAY 'none'.
           IF A NOT = 12 AND GREATER THAN 10 DISPLAY 'not 12, over 10'.
           IF A NOT = 12 AND 15 DISPLAY 'abbrev not =: no' ELSE DISPLAY 'abbrev not =: yes'.
           IF X = 'a' OR 'b' OR = 'q' DISPLAY 'q found' ELSE DISPLAY 'q missing'.
           IF A > B AND < C OR = 99 DISPLAY 'precedence ok' ELSE DISPLAY 'precedence wrong'.
           IF A < B OR > B + 4 AND < C - 1 DISPLAY 'expr ok' ELSE DISPLAY 'expr wrong'.
           IF (A > B) AND (C > A OR = 1) DISPLAY 'nested ok' ELSE DISPLAY 'nested wrong'.
           IF NOT A = B AND NOT C DISPLAY 'not-not ok' ELSE DISPLAY 'not-not wrong'.
           EVALUATE TRUE
               WHEN A > 20 OR < 5 MOVE 'out' TO R
               WHEN A = 15 OR 16 MOVE 'mid' TO R
               WHEN OTHER MOVE '???' TO R
           END-EVALUATE.
           DISPLAY R.
           EVALUATE A ALSO FLAG-ON ALSO TRUE
               WHEN 15 ALSO FALSE ALSO ANY MOVE 'off' TO R
               WHEN 15 ALSO TRUE ALSO B < C MOVE 'on ' TO R
               WHEN OTHER MOVE '???' TO R
           END-EVALUATE.
           DISPLAY R.
           STOP RUN.
