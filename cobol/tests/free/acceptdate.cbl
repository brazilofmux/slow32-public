       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCEPTDATE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  D6           PIC 9(6).
       01  D5           PIC 9(5).
       01  T8           PIC 9(8).
       01  W1           PIC 9.
       01  D-YY         PIC 99.
       01  MMDD         PIC 9(4).
       01  D-MM         PIC 99.
       01  D-DD         PIC 99.
       01  J-DDD        PIC 999.
       01  T-HH         PIC 99.
       01  CDT          PIC X(21).
       PROCEDURE DIVISION.
       MAIN.
           ACCEPT D6 FROM DATE.
           ACCEPT D5 FROM DAY.
           ACCEPT T8 FROM TIME.
           ACCEPT W1 FROM DAY-OF-WEEK.
           MOVE FUNCTION CURRENT-DATE TO CDT.
           DIVIDE D6 BY 10000 GIVING D-YY REMAINDER MMDD.
           DIVIDE MMDD BY 100 GIVING D-MM REMAINDER D-DD.
           IF D-MM >= 1 AND <= 12 AND D-DD >= 1 AND <= 31
               DISPLAY 'date ok' ELSE DISPLAY 'date bad ' D6.
           IF CDT (3:2) = D-YY DISPLAY 'year agrees with CURRENT-DATE'.
           DIVIDE D5 BY 1000 GIVING D-YY REMAINDER J-DDD.
           IF J-DDD >= 1 AND <= 366 DISPLAY 'day ok' ELSE DISPLAY 'day bad ' D5.
           DIVIDE T8 BY 1000000 GIVING T-HH.
           IF T-HH < 24 DISPLAY 'time ok' ELSE DISPLAY 'time bad ' T8.
           IF W1 >= 1 AND <= 7 DISPLAY 'day-of-week ok' ELSE DISPLAY 'dow bad ' W1.
           STOP RUN.
