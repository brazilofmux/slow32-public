* Index files kept up through changes, closed and opened again, walk as
* indexes built afresh walk.  An index is dBASE III's .NDX: the file
* keeps no links between its leaves, so opening rebuilds them from the
* tree; its pages are 512 bytes, so 900 records make trees three deep.
* Character, numeric and date keys, and one FOR index: appends, keys
* replaced, records moved into the FOR index and out of it (a record
* added or changed went into it whatever its condition said).
x = DBCREATE("NKP", {{"C", "C", 6, 0}, {"N", "N", 9, 2}, {"D", "D", 8, 0}})
USE NKP
i = 1
DO WHILE i <= 600
   APPEND BLANK
   REPLACE C WITH "k" + STR(MOD(i * 37, 211), 3), N WITH (MOD(i * 7919, 2003) - 1000) / 4, ;
      D WITH CTOD("01/01/86") + MOD(i * 13, 400)
   i = i + 1
ENDDO
INDEX ON C TO NKPC
INDEX ON N TO NKPN
INDEX ON D TO NKPD
INDEX ON C TO NKPF FOR N > 0
USE NKP INDEX NKPC, NKPN, NKPD, NKPF
* every fifth record takes new keys; three hundred more come
GO TOP
i = 1
DO WHILE i <= 600
   GO i
   IF MOD(i, 5) = 0
      REPLACE C WITH "r" + STR(MOD(i, 97), 3), N WITH -N, D WITH D + 30
   ENDIF
   i = i + 1
ENDDO
DO WHILE i <= 900
   APPEND BLANK
   REPLACE C WITH "k" + STR(MOD(i * 41, 211), 3), N WITH MOD(i, 50), D WITH CTOD("06/01/86") - MOD(i, 77)
   i = i + 1
ENDDO
USE
* opened again: each order's walk, summed by position
USE NKP INDEX NKPC, NKPN, NKPD, NKPF
s1 = walk(1)
s2 = walk(2)
s3 = walk(3)
s4 = walk(4)
USE
* built afresh
USE NKP
INDEX ON C TO NKPC2
INDEX ON N TO NKPN2
INDEX ON D TO NKPD2
INDEX ON C TO NKPF2 FOR N > 0
USE NKP INDEX NKPC2, NKPN2, NKPD2, NKPF2
? "char:", s1 = walk(1), "numeric:", s2 = walk(2), "date:", s3 = walk(3), "FOR:", s4 = walk(4)
? "FOR entries:", MOD(s4, 1000)
* and a SEEK of each kind
SET ORDER TO 1
SEEK "r  5"
? "seek char:", FOUND(), RECNO()
SET ORDER TO 2
SEEK 25
? "seek numeric:", FOUND(), RECNO()
SET ORDER TO 3
SEEK CTOD("05/01/86")
? "seek date:", FOUND(), RECNO()
USE
ERASE NKP.DBF
ERASE NKPC.NDX
ERASE NKPN.NDX
ERASE NKPD.NDX
ERASE NKPC2.NDX
ERASE NKPN2.NDX
ERASE NKPD2.NDX
ERASE NKPF.NDX
ERASE NKPF2.NDX

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
