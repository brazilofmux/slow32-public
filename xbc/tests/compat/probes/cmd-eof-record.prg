* Commands: the record past the end -- where SKIP, SEEK (with and
* without SOFTSEEK), DBSEEK, LOCATE, SUM, REPLACE ALL and LIST leave the
* pointer when they run off the end: RECNO() is LASTREC() + 1 and the
* fields read blank; LIST NEXT stops on its last record; DISPLAY there
* shows the blank record; REPLACE there keeps nothing.  REPLACE ALL with
* the scope first.  LIST and DISPLAY with no expressions: numbers only.
DBCREATE("cer_t", {{"K", "C", 3, 0}, {"Q", "N", 3, 0}})
USE cer_t
APPEND BLANK
REPLACE K WITH "ab", Q WITH 2
APPEND BLANK
REPLACE K WITH "cd", Q WITH 4
INDEX ON K TO cer_t
Show("start")
SEEK "xy"
Show("seek miss")
SEEK "bc"
Show("seek between")
SET SOFTSEEK ON
SEEK "bc"
Show("soft between")
SEEK "xy"
Show("soft past")
SET SOFTSEEK OFF
GO TOP
? DBSEEK("xy")
Show("dbseek miss")
GO TOP
LOCATE FOR K == "xy"
Show("locate miss")
GO TOP
SUM Q TO nSum
? nSum
Show("sum")
GO BOTTOM
SKIP
Show("skip")
SKIP -1
Show("skip back")
GO TOP
REPLACE ALL Q WITH Q + 10
Show("replace all")
GO BOTTOM
SKIP
REPLACE Q WITH 99
GO TOP
SUM Q TO nSum
? nSum, LASTREC()
GO TOP
LIST K
Show("list")
DISPLAY K, Q
GO TOP
LIST NEXT 1
Show("list next")
DISPLAY
USE
FERASE("cer_t.dbf")
FERASE("cer_t.ntx")
FERASE("cer_t.ndx")
RETURN

STATIC PROCEDURE Show(cWhat)
   ? cWhat, RECNO(), EOF(), FOUND(), "[" + K + "]", Q
RETURN
