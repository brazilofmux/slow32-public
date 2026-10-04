*> COB-CRT-STATUS (BP-G2, -dialect=gnucobol only): declared by no one,
*> GnuCOBOL's implicit CRT STATUS item, PIC 9(4), set by each ACCEPT
*> from the screen.  ACAS (cobol ISSUES-124) tests it against
*> COB-SCR-ESC after its prompts.  The ANSI stream is the expected output.
*> No oracle: GnuCOBOL's screens need a real tty.
identification division.
program-id. gnu-crtstatus.
data division.
working-storage section.
01  ws-in   pic x(3).
procedure division.
    accept ws-in at 0101
    display cob-crt-status at 0201
    accept ws-in at 0301
    display cob-crt-status at 0401
    stop run.
