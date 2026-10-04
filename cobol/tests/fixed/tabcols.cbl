       IDENTIFICATION DIVISION.
       PROGRAM-ID. TABCOLS.
      * A tab in reference format: spaces to the next stop of every
      * eight columns, as GnuCOBOL and Micro Focus read it (cobol
      * ISSUES-124: ACAS's mapser starts lines with one at column 7).
       PROCEDURE DIVISION.
000100	   DISPLAY "TAB AT COLUMN 7".
	   DISPLAY "TAB AT COLUMN 1".
           STOP RUN.
