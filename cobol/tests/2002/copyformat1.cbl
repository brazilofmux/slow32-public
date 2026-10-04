*> A SOURCE FORMAT directive on the first line of library text may be in
*> either form (2023 7.3.24.3 rule 4): this copybook, copied in fixed
*> form, says >>SOURCE FORMAT FREE from column 1, before the indicator
*> area.  (2002/copyformat has the carry-over and revert rules.)
*> No oracle: GnuCOBOL 4.0 refuses the directive there, which the standard allows.
identification division.
program-id. copyformat1.
data division.
working-storage section.
>>SOURCE FORMAT FIXED
       COPY "cpfmtfree1.cpy".
       01  WS-AFTER PIC X(5) VALUE "AFTER".
       PROCEDURE DIVISION.
           DISPLAY WS-FREE " " WS-AFTER.
           STOP RUN.
