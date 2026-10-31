* Statements: INIT PROCEDUREs run before the first procedure, EXIT
* PROCEDUREs as the program ends -- here by QUIT from a routine below
* it; ANNOUNCE, REQUEST and EXTERNAL above the first procedure leave it
* the entry point.  The INIT procedures' order is open (the guide), so
* only the one prints
ANNOUNCE StmtInitExit
REQUEST DBFNTX
EXTERNAL Tally
STATIC nRuns := 0

PROCEDURE Main
   ? "main after", nRuns, "inits"
   Leave()
   ? "not reached"
RETURN

STATIC PROCEDURE Leave
   ? "quitting"
   QUIT
RETURN

FUNCTION Tally()
RETURN nRuns

INIT PROCEDURE Opening
   nRuns++
   ? "opening", PCOUNT()
RETURN

INIT PROCEDURE Second
   nRuns++
RETURN

EXIT PROCEDURE Closing
   ? "closing", Tally()
RETURN
