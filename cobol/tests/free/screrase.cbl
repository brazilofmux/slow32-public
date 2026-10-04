*> ERASE in the SCREEN SECTION (2023 13.18.21): on DISPLAY, clearing
*> from the entry's position to the end of the line (EOL, END OF LINE)
*> or of the screen (EOS, END OF SCREEN) before the entry is painted; an
*> ERASE on the 01 clears from its first field's position; during an
*> ACCEPT of the screen every ERASE is ignored (rule 2).  ACAS (cobol
*> ISSUES-124) puts ERASE EOS on its screens' 01.  The keys come from
*> screrase.keys; the ANSI stream is the expected output.
*> No oracle: GnuCOBOL's screens need a real tty.
identification division.
program-id. screrase.
data division.
working-storage section.
01  ws-in  pic x(2) value spaces.
screen section.
01  s1  erase eos.
    03  value "TOP"           line 2 col 1.
    03  value "ROW"           line 3 col 5 erase eol.
    03  value "END"           line 4 col 1 erase end of line.
    03  pic x(2) to ws-in     line 5 col 1.
procedure division.
    display s1
    accept s1
    stop run.
