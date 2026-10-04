*> ACCEPT and DISPLAY of a screen-name AT its origin: AT 0101, or AT
*> LINE 1 COLUMN 1, places the screen where its own clauses put it.
*> ACAS's IRS (cobol ISSUES-124) displays its heading screen AT 0101.
*> The keys come from scrat.keys; the ANSI stream is the expected output.
*> No oracle: GnuCOBOL's screens need a real tty.
identification division.
program-id. scrat.
data division.
working-storage section.
01  ws-in  pic x(3) value spaces.
screen section.
01  heading-screen.
    03  value "HEADING"   line 1 col 1.
01  entry-screen.
    03  value "CODE:"     line 3 col 1.
    03  pic x(3) to ws-in line 3 col 7.
procedure division.
    display heading-screen at 0101
    accept entry-screen at line 1 column 1
    display ws-in at 0501
    stop run.
