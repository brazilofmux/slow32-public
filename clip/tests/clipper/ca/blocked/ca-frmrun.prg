* CA: SYS/FRMRUN.PRG SYS/FRMBACK.PRG
* CA's REPORT FORM runner over a report form our interpreter's CREATE
* REPORT wrote (ca-frmrun.frm beside this driver: a dBASE III .frm, which
* Clipper's reader shares): a PLAIN report, then a SUMMARY one.
DBCREATE("staff", {{"FIRST", "C", 12, 0}, {"LAST", "C", 12, 0}, {"AGE", "N", 3, 0}, {"SALARY", "N", 10, 2}})
USE staff
APPEND BLANK
REPLACE FIRST WITH "Ada", LAST WITH "Byron", AGE WITH 36, SALARY WITH 52000
APPEND BLANK
REPLACE FIRST WITH "Grace", LAST WITH "Hopper", AGE WITH 45, SALARY WITH 61000.50
APPEND BLANK
REPLACE FIRST WITH "Linus", LAST WITH "Pauling", AGE WITH 22, SALARY WITH 23500
GO TOP
__ReportForm("ca-frmrun.frm", .F., "", .F., NIL, NIL, NIL, NIL, .F., .T., "", .F., .F.)
? "--- summary"
GO TOP
__ReportForm("ca-frmrun.frm", .F., "", .F., NIL, NIL, NIL, NIL, .F., .T., "", .F., .T.)
