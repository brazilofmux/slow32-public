* Field GETs: a date field edits as a date (accepted, it was stored back
* as raw text and came back blank), and a GET left as it began is not
* assigned (a blank "999-9999" field keeps its blanks, not "   -    ")
x = DBCREATE("FLDT", {{"P", "C", 8, 0}, {"D", "D", 8, 0}})
USE FLDT
APPEND BLANK
REPLACE D WITH CTOD("01/24/94")
@ 1,1 GET D
@ 2,1 GET P PICTURE "999-9999"
READ
? DTOC(D) + "[" + P + "]"
USE
ERASE FLDT.DBF
QUIT
