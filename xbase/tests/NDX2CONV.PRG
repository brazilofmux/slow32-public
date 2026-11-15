* Index files in this interpreter's own format before dBASE III's
* (NDX2, tests/ndx2: made by the build before the change) are converted
* when opened, and walk as indexes built afresh walk: character, numeric
* and date keys, DESCENDING and FOR kept
SET TALK OFF
SET SAFETY OFF
USE OLDIX INDEX OLDC, OLDN, OLDD, OLDCD, OLDCF
o1 = walk(1)
o2 = walk(2)
o3 = walk(3)
o4 = walk(4)
o5 = walk(5)
USE
USE OLDIX
INDEX ON C TO NEWC
INDEX ON N TO NEWN
INDEX ON D TO NEWD
INDEX ON C TO NEWCD DESCENDING
INDEX ON C TO NEWCF FOR N > 0
USE OLDIX INDEX NEWC, NEWN, NEWD, NEWCD, NEWCF
? "converted:", o1 = walk(1), o2 = walk(2), o3 = walk(3), o4 = walk(4), o5 = walk(5)
? "FOR kept:", MOD(o5, 1000) < 400
USE
RETURN

FUNCTION walk
PARAMETERS o
PRIVATE s, k
SET ORDER TO o
s = 0
k = 0
GO TOP
DO WHILE .NOT. EOF()
   k = k + 1
   s = MOD(s + RECNO() * k, 1000003)
   SKIP
ENDDO
RETURN s * 1000 + k
