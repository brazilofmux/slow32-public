*> The format library text starts in is the one in effect at its COPY
*> (2023 7.3.24.3 rule 3), and a SOURCE FORMAT directive inside library
*> text lasts only to its end (rule 5).  This program turns FIXED; a
*> fixed copybook follows; another copybook turns itself FREE, and the
*> program after it is fixed again.  ACAS (cobol ISSUES-124) starts every
*> program >>SOURCE FREE and writes its copybooks free, with no
*> directive of their own.
identification division.
program-id. copyformat.
data division.
working-storage section.
>>SOURCE FORMAT FIXED
       COPY "cpfmtfix.cpy".
       COPY "cpfmtfree.cpy".
       01  WS-AFTER PIC X(5) VALUE "AFTER".
       PROCEDURE DIVISION.
           DISPLAY WS-FIX " " WS-FREE " " WS-AFTER.
           STOP RUN.
