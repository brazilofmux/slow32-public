*> ACCEPT ... FROM LINES and FROM COLUMNS (X/Open's; BP-E32): the
*> terminal's size, 24 by 80 when there is no terminal -- checked here
*> only as positive.  GnuCOBOL starts curses for them and, with no
*> terminal, stops the program (docs/oracles.md).  ACAS (cobol
*> ISSUES-124) sizes its screens from them.
identification division.
program-id. mf-scrdims.
data division.
working-storage section.
01  ws-lines   pic 9(3).
01  ws-cols    pic 9(3).
procedure division.
    accept ws-lines from lines.
    accept ws-cols from columns.
    if ws-lines > 0 and ws-cols > 0
        display "size known"
    else
        display "size unknown"
    end-if.
    goback.
