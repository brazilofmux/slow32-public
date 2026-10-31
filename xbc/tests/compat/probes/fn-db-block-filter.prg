* Functions: DBSETFILTER and DBSETRELAT given only a block -- the block is
* the condition, DBFILTER() the text (none given: "").  DBRELATION() with
* no text is "" in Clipper and NIL in Harbour, so it is not shown
LOCAL nMin := 25, i, aSeen
DBCREATE("probe_kids", {{"PID", "N", 3, 0}, {"NAME", "C", 6, 0}})
DBCREATE("probe_dads", {{"ID", "N", 3, 0}, {"QTY", "N", 5, 0}})
DBUSEAREA(.T., , "probe_kids", "KIDS")
FOR i := 1 TO 4
   DBAPPEND()
   FIELDPUT(1, 5 - i)
   FIELDPUT(2, "kid" + STR(5 - i, 1))
NEXT
INDEX ON PID TO probe_kids
DBUSEAREA(.T., , "probe_dads", "DADS")
FOR i := 1 TO 5
   DBAPPEND()
   FIELDPUT(1, i)
   FIELDPUT(2, i * 10)
NEXT
DBSETFILTER({|| QTY > nMin})
? "[" + DBFILTER() + "]"
? Walk()
nMin := 35
? Walk()
DBSETFILTER(Above(15), "QTY > 15")
? "[" + DBFILTER() + "]", Walk()
SET FILTER TO QTY < 30
? "[" + DBFILTER() + "]", Walk()
DBCLEARFILTER()
? "[" + DBFILTER() + "]", Walk()
DBSETRELAT("KIDS", {|| ID - 1})
? DBRSELECT(1)
aSeen := {}
DBGOTOP()
DO WHILE !EOF()
   AADD(aSeen, STR(ID, 1) + ":" + IF(KIDS->(EOF()), "-", TRIM(KIDS->NAME)))
   DBSKIP()
ENDDO
? aSeen[1], aSeen[2], aSeen[3], aSeen[4], aSeen[5]
DBCLEARRELATION()
? DBRSELECT(1)
DBCLOSEALL()
FERASE("probe_kids.dbf")
FERASE("probe_dads.dbf")
FERASE("probe_kids.ntx")
FERASE("probe_kids.ndx")

FUNCTION Walk()
   LOCAL c := ""
   DBGOTOP()
   DO WHILE !EOF()
      c += STR(ID, 1)
      DBSKIP()
   ENDDO
   RETURN c

FUNCTION Above(n)
   LOCAL nAt := n
   RETURN {|| QTY > nAt}
