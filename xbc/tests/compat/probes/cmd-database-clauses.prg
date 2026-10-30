* Commands: the database commands' clauses -- scopes, FOR / WHILE, NEW /
* ALIAS, INDEX ON's clauses, relations, COPY / APPEND forms, SORT, TOTAL
LOCAL n := 0, s := 0, a := 0
DBCREATE("cdc_items", {{"CODE", "C", 3, 0}, {"GRP", "C", 1, 0}, {"QTY", "N", 5, 0}})
USE cdc_items NEW ALIAS it
FOR n := 1 TO 6
   APPEND BLANK
   REPLACE CODE WITH "C" + STR(n, 1), GRP WITH IIF(n % 2 == 0, "E", "O"), QTY WITH n * 10
NEXT
? ALIAS(), RECCOUNT(), SELECT()
COUNT TO n FOR GRP == "E"
? "even", n
SUM QTY TO s FOR QTY > 20
? "sum", s
AVERAGE QTY TO a
? "avg", a
GO TOP
REPLACE QTY WITH QTY + 1 NEXT 2
GO 4
REPLACE QTY WITH 0 REST FOR GRP == "O"
GO TOP
LIST CODE, GRP, QTY OFF
LOCATE FOR QTY == 0
? "located", FOUND(), RECNO()
CONTINUE
? "continued", FOUND(), RECNO()
CONTINUE
? "ended", FOUND(), EOF()
INDEX ON QTY TO cdc_q DESCENDING
GO TOP
? "top by qty desc", CODE, QTY
INDEX ON CODE TO cdc_c FOR GRP == "E"
GO TOP
COUNT TO n
? "indexed for", n
SET INDEX TO cdc_q, cdc_c
SET ORDER TO 2
GO TOP
? "order 2", CODE, INDEXORD()
SET ORDER TO 0
SET INDEX TO
DELETE RECORD 2
DELETE FOR QTY == 0
COUNT TO n FOR DELETED()
? "deleted", n
RECALL ALL
COUNT TO n FOR DELETED()
? "recalled", n
DELETE RECORD 1
PACK
? "packed", RECCOUNT(), DELETED()
COPY TO cdc_copy FIELDS CODE, QTY FOR GRP == "E"
COPY TO cdc_text DELIMITED
COPY TO cdc_sdf SDF FIELDS CODE
COPY STRUCTURE TO cdc_empty
SELECT 0
USE cdc_copy
? "copy", RECCOUNT(), FCOUNT(), FIELD(2)
USE cdc_empty
? "structure", RECCOUNT(), FCOUNT()
APPEND FROM cdc_text DELIMITED
? "from delimited", RECCOUNT(), CODE
ZAP
? "zapped", RECCOUNT()
APPEND FROM cdc_sdf SDF
? "from sdf", RECCOUNT(), CODE, QTY
USE
SELECT it
SORT TO cdc_sorted ON QTY /D
USE cdc_sorted NEW
? "sorted", CODE, QTY
USE
SELECT it
TOTAL ON GRP TO cdc_total FIELDS QTY
USE cdc_total NEW
GO TOP
? "total", GRP, QTY
USE
SELECT it
GO 2
SKIP
? "skipped", RECNO(), it->CODE
SKIP -1 ALIAS it
? "back", RECNO()
STORE 5 TO n, s
? n, s
RELEASE ALL LIKE s*
CLOSE DATABASES
? "closed", USED(), SELECT()
FERASE("cdc_items.dbf")
FERASE("cdc_copy.dbf")
FERASE("cdc_empty.dbf")
FERASE("cdc_sorted.dbf")
FERASE("cdc_total.dbf")
FERASE("cdc_text.txt")
FERASE("cdc_sdf.txt")
FERASE("cdc_q.ndx")
FERASE("cdc_c.ndx")
FERASE("cdc_q.ntx")
FERASE("cdc_c.ntx")
