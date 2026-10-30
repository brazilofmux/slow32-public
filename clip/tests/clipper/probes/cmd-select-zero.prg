* Commands: SELECT 0 is the first unused work area; USE ... ALIAS there,
* and the record pointer after USE, GO TOP and a SKIP loop
LOCAL i
DBCREATE("csz_t", {{"N", "N", 2, 0}})
SELECT 0
USE csz_t ALIAS tt
APPEND BLANK
REPLACE N WITH 7
GO TOP
? SELECT(), ALIAS(), RECNO(), RECCOUNT(), LASTREC(), BOF(), EOF()
FOR i := 1 TO RECCOUNT()
   SKIP
NEXT
? RECNO(), EOF()
GO TOP
? RECNO(), BOF(), EOF()
SELECT 0
? SELECT(), ALIAS(), RECNO()
CLOSE DATABASES
FERASE("csz_t.dbf")
