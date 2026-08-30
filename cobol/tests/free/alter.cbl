       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALTER1.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  N            PIC 9 VALUE 0.
       PROCEDURE DIVISION.
       MAIN.
           PERFORM SWITCH-PARA THRU DONE-WAY.
           ALTER SWITCH-PARA TO PROCEED TO SECOND-WAY.
           PERFORM SWITCH-PARA THRU DONE-WAY.
           ALTER SWITCH-PARA TO FIRST-WAY BARE-PARA TO THIRD-WAY.
           PERFORM SWITCH-PARA THRU DONE-WAY.
           PERFORM BARE-PARA THRU DONE-WAY.
           DISPLAY 'n=' N.
           STOP 'operator, carry on'.
           STOP RUN.
       SWITCH-PARA.
           GO TO FIRST-WAY.
       BARE-PARA.
           GO TO.
       FIRST-WAY.
           DISPLAY 'first'.
           GO TO DONE-WAY.
       SECOND-WAY.
           DISPLAY 'second'.
           GO TO DONE-WAY.
       THIRD-WAY.
           DISPLAY 'third'.
           ADD 1 TO N.
       DONE-WAY.
           EXIT.
