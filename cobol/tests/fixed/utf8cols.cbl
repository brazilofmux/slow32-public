       IDENTIFICATION DIVISION.
       PROGRAM-ID. UTF8COLS.
      * Reference-format columns count characters (code points), so a
      * card image keeps its layout when its text is UTF-8 (cobol
      * ISSUES-63; the user's ruling).  The DISPLAY literal ends at column
      * 72 counted in characters, with a sequence-area tag in 73-80;
      * counted in bytes it would run into the sequence area.  No oracle:
      * GnuCOBOL counts bytes, as -fixed-columns=bytes does here.
       PROCEDURE DIVISION.
           DISPLAY "Grüße, café, ñandú................................."CHAR0072
           DISPLAY "the tag in columns 73-80 was ignored"               ÉÉÉÉÉÉÉÉ
           STOP RUN.
