       IDENTIFICATION DIVISION.
       PROGRAM-ID. RMEND.
      * Three things RM/COBOL's reader and parser let through that the
      * Open Systems AP and IN modules use: a bare END PROGRAM., a
      * doubled period after a VALUE, and COMP-1 as a two-byte binary
      * integer with a PICTURE (a float elsewhere). No oracle: GnuCOBOL's
      * COMP-1 is a float in every dialect it offers here.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JCL-CODE  VALUE ZERO   COMP-1  PIC S999.
       77  TASK-NO   VALUE 7      COMP-1  PIC S99.
       01  DATES.
           05  FILLER   PIC 9(8)  VALUE 12370121..
           05  DUE-DATE PIC 9(8)  VALUE 12370125.
       PROCEDURE DIVISION.
       MAIN.
           ADD 240 TO JCL-CODE.
           MULTIPLY 3 BY TASK-NO.
           DISPLAY "JCL-CODE=" JCL-CODE " TASK-NO=" TASK-NO
               " DUE=" DUE-DATE.
           STOP RUN.
       END PROGRAM.
