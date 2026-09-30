      *> COMP-1 and COMP-2 (docs/usage.md; default dialect: they are
      *> Micro Focus's): IEEE floats, shown in MF's -.9(8)E-99 form.
      *> GnuCOBOL computes the same values and shows them its own way
      *> (.oracle-expected, docs/oracles.md).
       IDENTIFICATION DIVISION.
       PROGRAM-ID. comp12.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 s1       COMP-1 VALUE 1.5.
       01 s2       COMP-1 VALUE -0.25.
       01 d1       COMP-2 VALUE 1234567.875.
       01 d2       COMP-2.
       01 dec      PIC S9(5)V99.
       01 ed       PIC -ZZ,ZZ9.999.
       01 k        PIC 9.
       PROCEDURE DIVISION.
           DISPLAY "lengths " FUNCTION LENGTH(s1) " " FUNCTION LENGTH(d1)

           DISPLAY s1 "|" s2 "|" d1
           MOVE 0 TO d2 DISPLAY d2
           MOVE 0.0001 TO d2 DISPLAY d2
           MOVE -12345.67 TO d2 DISPLAY d2
           COMPUTE d2 = s1 * 3 + s2 DISPLAY d2
           MOVE d2 TO dec DISPLAY dec
           COMPUTE dec ROUNDED = 2 / 3 * s1 DISPLAY dec
           COMPUTE dec = 2 / 3 * s1 DISPLAY dec
           MOVE d1 TO ed DISPLAY ed
           COMPUTE d2 = 2 ** 0.5 DISPLAY d2
           COMPUTE dec = 9 ** 0.5 DISPLAY dec
           COMPUTE dec = 2 ** -2 DISPLAY dec
           DIVIDE 4 INTO d1 DISPLAY d1
           ADD 1 TO s1 DISPLAY s1
           SUBTRACT d1 FROM s2 DISPLAY s2
           IF s1 = 2.5 AND s1 > 2.4999 AND s2 < 0 AND d1 NOT = 0
               DISPLAY "compare ok"
           END-IF
           MOVE s1 TO d2
           IF d2 = s1 DISPLAY "float to double exact" END-IF
           PERFORM VARYING k FROM 1 BY 1 UNTIL k > 3
               COMPUTE d2 = d2 * 10
           END-PERFORM
           DISPLAY d2
           MOVE d2 TO k DISPLAY k
           INITIALIZE s1 d2 DISPLAY s1 "|" d2
           STOP RUN.
