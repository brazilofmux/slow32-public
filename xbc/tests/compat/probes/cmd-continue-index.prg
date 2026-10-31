// CONTINUE goes on from the record LOCATE found in the controlling
// index's order, not the physical next one (it found that same record
// again, forever).
PROCEDURE Main()
   LOCAL i := 0
   DBCREATE("ci", { { "K", "C", 1, 0 }, { "D", "D", 8, 0 }, { "P", "N", 3, 0 } })
   USE ci
   APPEND BLANK ; REPLACE K WITH "1", D WITH CTOD("01/05/87"), P WITH 10
   APPEND BLANK ; REPLACE K WITH "2", D WITH CTOD("01/02/87"), P WITH 20
   APPEND BLANK ; REPLACE K WITH "1", D WITH CTOD("01/03/87"), P WITH 30
   APPEND BLANK ; REPLACE K WITH "1", D WITH CTOD("01/04/87"), P WITH 40
   INDEX ON K+STR(D-CTOD("01/01/80"),6) TO cix
   LOCATE FOR K = "1"
   DO WHILE K = "1" .AND. .NOT. EOF() .AND. i < 8
      ? RECNO(), P
      i++
      CONTINUE
   ENDDO
   ? "done", i, EOF()
   USE
RETURN
