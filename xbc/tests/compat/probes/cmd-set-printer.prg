* Commands: the printer -- SET PRINTER TO a file, @ ... SAY under SET
* DEVICE TO PRINTER, ? under SET PRINTER ON, EJECT, PROW() and PCOL(),
* ADDITIVE, SET CONSOLE OFF; the page shown with | for a new line and
* <FF> for a form feed
LOCAL cPage
SET PRINTER TO csp_out.txt
SET DEVICE TO PRINTER
@ 1, 4 SAY "Title"
@ 1, 20 SAY "p1"
@ 3, 0 SAY 42
? "at", PROW(), PCOL()
@ 2, 2 SAY "page two"
? "after eject", PROW(), PCOL()
DEVPOS(4, 6)
DEVOUT("devout")
SET DEVICE TO SCREEN
? "screen again", PROW(), PCOL(), SET(_SET_DEVICE)
SET PRINTER ON
? "both"
SET CONSOLE OFF
? "printer only"
SET CONSOLE ON
SET PRINTER OFF
? "console only", SET(_SET_PRINTER)
EJECT
? "ejected", PROW(), PCOL()
SET PRINTER TO
? Page("csp_out.txt")
SET PRINTER TO csp_out ADDITIVE
SET PRINTER ON
SET CONSOLE OFF
?? "more"
SET CONSOLE ON
SET PRINTER OFF
SET PRINTER TO
? Page("csp_out.txt")
? FILE("csp_out.prn")
FERASE("csp_out.txt")
FERASE("csp_out.prn")
RETURN

STATIC FUNCTION Page(cFile)
RETURN STRTRAN(STRTRAN(MEMOREAD(cFile), CHR(10), "|"), CHR(12), "<FF>")
