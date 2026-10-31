// An empty table: USE leaves EOF() and BOF() true; GO TOP and GO BOTTOM,
// filtered or not, are no error (they were "No records") and leave the
// pointer on the record past the end.
PROCEDURE Main()
   DBCREATE("empt", { { "D", "D", 8, 0 } })
   USE empt
   ? RECCOUNT(), EOF(), BOF()
   GO TOP
   ? "top", RECNO(), EOF(), BOF()
   SET FILTER TO D >= DATE()
   GO TOP
   ? "filtered top", RECNO(), EOF(), BOF()
   GO BOTTOM
   ? "bottom", RECNO(), EOF(), BOF()
   SET FILTER TO
RETURN
