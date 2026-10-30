* Functions: the DB*() forms over a table DBCREATE makes
LOCAL b, i, total := 0
DBCREATE("probe_items", {{"CODE", "C", 3, 0}, {"QTY", "N", 5, 0}, {"WHEN", "D", 8, 0}, {"OK", "L", 1, 0}})
DBUSEAREA(.T., , "probe_items", "ITEMS")
? ALIAS(), LASTREC(), FCOUNT(), FIELDNAME(2), FIELDNAME(5)
FOR i := 1 TO 5
   DBAPPEND()
   FIELDPUT(1, "C" + STR(6 - i, 1))
   FIELDPUT(2, i * 10)
   FIELDPUT(4, i % 2 == 0)
NEXT
? LASTREC(), RECNO(), FIELDGET(1), FIELDGET(2), FIELDGET(4)
DBGOTO(2)
? RECNO(), FIELDGET(1)
b := FIELDBLOCK("QTY")
? EVAL(b), EVAL(b, 99), QTY
INDEX ON CODE TO probe_items
? INDEXORD(), INDEXKEY(), INDEXKEY(1)
DBGOTOP()
? RECNO(), CODE
? DBSEEK("C3"), RECNO(), QTY
? DBSEEK("ZZ"), FOUND()
DBSETORDER(0)
DBGOTOP()
? RECNO(), CODE
DBEVAL({|| total += QTY})
? "total", total
total := 0
DBEVAL({|| total += QTY}, {|| OK})
? "even", total
total := 0
DBEVAL({|| total += 1}, , , 2)
? "two", total
DBGOTO(1)
DBDELETE()
? DELETED()
DBRECALL()
? DELETED()
DBSETFILTER({|| QTY > 30}, "QTY > 30")
? DBFILTER()
DBGOTOP()
? RECNO()
DBCLEARFIL()
? DBFILTER() == "", DBSETDRIVER() <> ""
DBSELECTAR(2)
? SELECT(), ALIAS()
DBSELECTAR("ITEMS")
? SELECT(), ALIAS()
DBCLOSEAREA()
? USED(), ALIAS() == ""
FERASE("probe_items.dbf")
FERASE("probe_items.ndx")
FERASE("probe_items.ntx")
