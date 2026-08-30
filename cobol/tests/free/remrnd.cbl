       IDENTIFICATION DIVISION.
       PROGRAM-ID. REMRND.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  A            PIC 9(3) VALUE 100.
       01  B            PIC 9(2) VALUE 7.
       01  Q0           PIC 9(3).
       01  Q1           PIC 9(3)V9.
       01  R0           PIC 9(3)V99.
       01  R1           PIC S9(3)V99.
       01  N            PIC S9(3)V99 VALUE -17.5.
       01  QN           PIC S9(2)V9.
       01  RN           PIC S9(2)V999.
       PROCEDURE DIVISION.
       MAIN.
           DIVIDE A BY B GIVING Q0 REMAINDER R0.
           DISPLAY Q0 ' ' R0.
           DIVIDE A BY B GIVING Q0 ROUNDED REMAINDER R0.
           DISPLAY Q0 ' ' R0.
           DIVIDE A BY B GIVING Q1 ROUNDED REMAINDER R1.
           DISPLAY Q1 ' ' R1.
           DIVIDE B INTO A GIVING Q1 REMAINDER R1.
           DISPLAY Q1 ' ' R1.
           DIVIDE N BY 3 GIVING QN ROUNDED REMAINDER RN.
           DISPLAY QN ' ' RN.
           DIVIDE 3 INTO N GIVING QN REMAINDER RN.
           DISPLAY QN ' ' RN.
           MOVE 999 TO A.
           MOVE 1 TO B.
           MOVE 5 TO Q0.
           MOVE 1 TO R0.
           DIVIDE A BY B GIVING Q0 REMAINDER R0
               ON SIZE ERROR DISPLAY 'size error (quotient fits)'.
           DISPLAY Q0 ' ' R0.
           MOVE 7 TO B.
           MOVE 1 TO R0.
           DIVIDE A BY B GIVING Q1 ROUNDED REMAINDER R0
               ON SIZE ERROR DISPLAY 'size error on the quotient'
               NOT ON SIZE ERROR DISPLAY 'no size error'.
           DISPLAY Q1 ' ' R0.
           STOP RUN.
