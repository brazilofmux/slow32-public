*> Colours on a positioned DISPLAY and ACCEPT (BP-E7): FOREGROUND-COLOR
*> and BACKGROUND-COLOR as a number, and as a level 78 constant (which
*> arrives as its number), as in the SCREEN SECTION.  ACAS (cobol
*> ISSUES-124) colours most of its prompts this way, from GnuCOBOL's
*> screenio.cpy names.  The ANSI stream is the expected output.
*> No oracle: GnuCOBOL's screens need a real tty.
identification division.
program-id. poscolor.
data division.
working-storage section.
78  cob-color-green   value 2.
78  cob-color-blue    value 1.
01  ws-name   pic x(5) value "ACAS".
01  ws-in     pic x(3).
procedure division.
    display "Day Book" at 0136 with foreground-color 2
    display ws-name at line 2 col 1 with foreground-color cob-color-green
                                          background-color cob-color-blue
    display "plain" at 0301
    accept ws-in at 0401 with foreground-color 6 auto-skip
    display ws-in at 0501
    stop run.
