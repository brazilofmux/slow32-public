// GO n puts the pointer on a record; SKIP and a REST scope then go on from
// it in the controlling index's order (the index's own place had stayed
// where it was before the GO).
PROCEDURE Main()
   LOCAL n := 0
   DBCREATE("gs", { { "K", "C", 1, 0 } })
   USE gs
   APPEND BLANK ; REPLACE K WITH "C"
   APPEND BLANK ; REPLACE K WITH "A"
   APPEND BLANK ; REPLACE K WITH "D"
   APPEND BLANK ; REPLACE K WITH "B"
   INDEX ON K TO gsx
   GO 2
   SKIP
   ? RECNO(), K
   GO 1
   SKIP
   ? RECNO(), K
   GO 3
   SKIP -1
   ? RECNO(), K
   GO 4
   COUNT REST TO n
   ? "rest from B", n
   INDEX ON K TO gsd DESCENDING
   GO 1
   SKIP
   ? RECNO(), K
   USE
RETURN
