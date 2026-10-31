* The entry point: the first procedure, when every line above it only
* declares -- ANNOUNCE, REQUEST, EXTERNAL, MEMVAR, comments
ANNOUNCE EntryDecl
REQUEST DBFNTX
EXTERNAL Doubled
MEMVAR nSeen

PROCEDURE Main
   PUBLIC nSeen := 3
   ? "entered", Doubled(nSeen)
RETURN

FUNCTION Doubled(n)
RETURN n * 2
