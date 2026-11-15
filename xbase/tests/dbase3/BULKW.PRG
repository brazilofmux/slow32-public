* dBASE's own index of BULK (BULKC.NDX here, copied as BULKG) walks in
* key order: its last leaf, one level up, last (a level-by-level walk put
* it first)
SET TALK OFF
USE BULK INDEX BULKG
k = 0
ok = .T.
p = ""
GO TOP
DO WHILE .NOT. EOF()
   k = k + 1
   IF C < p
      ok = .F.
   ENDIF
   p = C
   SKIP
ENDDO
GO BOTTOM
? "dBASE's walk:", k, ok, C
USE
