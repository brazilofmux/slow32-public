* CA: SYS/LBLRUN.PRG SYS/LBLBACK.PRG
* CA's LABEL FORM runner over a label form our interpreter's CREATE LABEL
* wrote (ca-lblrun.lbl beside this driver: dBASE III's .lbl layout, which
* Clipper's reader shares): the labels, then the SAMPLE form.
PROCEDURE Main
DBCREATE("staff", {{"NAME", "C", 15, 0}, {"CITY", "C", 12, 0}, {"STATE", "C", 2, 0}})
USE staff
APPEND BLANK
REPLACE NAME WITH "Ada Byron", CITY WITH "London", STATE WITH "UK"
APPEND BLANK
REPLACE NAME WITH "Grace Hopper", CITY WITH "New York", STATE WITH "NY"
APPEND BLANK
REPLACE NAME WITH "Linus Pauling", CITY WITH "Portland", STATE WITH "OR"
GO TOP
__LabelForm("ca-lblrun.lbl", .F., "", .F., NIL, NIL, NIL, NIL, .F., .F.)
? "--- two across is the form's own business; done"
RETURN
