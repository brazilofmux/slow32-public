// AVERAGE over no records -- an empty table, or a FOR none meets -- is 0,
// no error (it was "No records").
PROCEDURE Main()
   LOCAL n := -1
   DBCREATE("avgt", { { "N", "N", 5, 0 } })
   USE avgt
   AVERAGE N TO n
   ? "empty", n
   APPEND BLANK ; REPLACE N WITH 4
   AVERAGE N TO n FOR N > 10
   ? "none match", n
RETURN
