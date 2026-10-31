* Commands: LIST and DISPLAY on the console stream -- TO PRINTER, the
* alternate file, SET CONSOLE OFF -- and SET MARGIN on the page (it
* counts in PCOL()); the page shown with | for a new line
DBCREATE("clp_t", {{"K", "C", 3, 0}, {"Q", "N", 3, 0}})
USE clp_t
APPEND BLANK
REPLACE K WITH "aa", Q WITH 1
APPEND BLANK
REPLACE K WITH "bb", Q WITH 2
SET PRINTER TO clp_out.txt
SET MARGIN TO 3
SET DEVICE TO PRINTER
@ 1, 2 SAY "x"
? "pc", PCOL()
@ 2, 0 SAY "y"
? "pc", PCOL()
SET DEVICE TO SCREEN
SET PRINTER ON
? "z", PCOL()
SET PRINTER OFF
LIST K, Q TO PRINTER
SET MARGIN TO 0
SET ALTERNATE TO clp_alt.txt
SET ALTERNATE ON
LIST K
GO TOP
DISPLAY Q
SET ALTERNATE OFF
SET ALTERNATE TO
SET PRINTER TO
SET CONSOLE OFF
LIST K TO FILE clp_lst
SET CONSOLE ON
? "lst", STRTRAN(MEMOREAD("clp_lst.txt"), CHR(10), "|")
? "pg", STRTRAN(STRTRAN(MEMOREAD("clp_out.txt"), CHR(10), "|"), CHR(13), "<CR>")
? "alt", STRTRAN(MEMOREAD("clp_alt.txt"), CHR(10), "|")
USE
FERASE("clp_t.dbf")
FERASE("clp_out.txt")
FERASE("clp_alt.txt")
FERASE("clp_lst.txt")
