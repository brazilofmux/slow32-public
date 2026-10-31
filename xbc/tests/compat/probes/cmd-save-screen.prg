* Commands: SAVE SCREEN TO a variable (made PRIVATE when there is none,
* so a routine it calls sees it; a LOCAL kept as one), RESTORE SCREEN
* FROM it, and the two without a variable -- one buffer of their own
PROCEDURE Main
   SAVE SCREEN TO cWhole
   ? "saved", VALTYPE(cWhole), Seen()
   RESTORE SCREEN FROM cWhole
   ? "restored"
   Kept()
   SAVE SCREEN
   RESTORE SCREEN
   ? "bare"
RETURN

STATIC FUNCTION Seen()
RETURN TYPE("cWhole")

STATIC PROCEDURE Kept()
   LOCAL cMine
   SAVE SCREEN TO cMine
   ? "local", VALTYPE(cMine)
   RESTORE SCREEN FROM cMine
RETURN
