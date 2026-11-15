* dBASE III PLUS's own index maintenance, to be matched byte for byte:
* splits of leaves and internal pages (a 30-byte key: 12 to a page),
* equal keys in the order they come, a key's last entry moved off its
* leaf, leaves emptied, blank dates (after every date), a compound key
SET TALK OFF
SET SAFETY OFF
USE NDXK
INDEX ON C TO NDXKC
INDEX ON N TO NDXKN
INDEX ON D TO NDXKD
INDEX ON K + DTOC(D) TO NDXKX
USE NDXK INDEX NDXKC, NDXKN, NDXKD, NDXKX
i = 1
DO WHILE i <= 300
   APPEND BLANK
   REPLACE C WITH "k" + STR(MOD(i * 37, 61), 3), N WITH MOD(i * 13, 41) - 20
   IF MOD(i, 9) <> 0
      REPLACE D WITH CTOD("03/01/86") + MOD(i * 7, 53)
   ENDIF
   REPLACE K WITH STR(MOD(i * 11, 17), 2)
   i = i + 1
ENDDO
* every record whose key is in k 10 to k 29 moves to the end: leaves empty
i = 1
DO WHILE i <= 300
   GO i
   IF C >= "k 10" .AND. C < "k 30"
      REPLACE C WITH "z" + SUBSTR(C, 2), N WITH N + 100
   ENDIF
   i = i + 1
ENDDO
* some move back, ahead of records already there; a few dates blank
i = 7
DO WHILE i <= 300
   GO i
   REPLACE C WITH "k" + SUBSTR(C, 2), D WITH CTOD("  /  /  ")
   i = i + 23
ENDDO
USE
QUIT
