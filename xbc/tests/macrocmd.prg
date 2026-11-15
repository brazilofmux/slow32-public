* Commands with a macro where dBASE programs put one, and other things
* the lab's correlation program (Quicksilver-built) leaned on:
* REPLACE &fld WITH ... (the field part of the line was read from the
* macro's expansion: the field and nothing more), FOR &c and WHILE &c
* (the rest of the line was lost), INDEX ON &k (a length gone negative:
* memory overwritten), SUM and AVERAGE of a list TO a list (one taken),
* a name past ten characters for its field (p_URIC_ACID: P_URIC_ACI),
* CLOSE ALL keeping the variables (it released them), MOD(x, 0) = x.
x = DBCREATE("MCMD", {{"P_URIC_ACI", "N", 5, 0}, {"F", "N", 6, 2}, {"C", "C", 4, 0}})
USE MCMD
i = 1
DO WHILE i <= 6
   APPEND BLANK
   REPLACE P_URIC_ACI WITH i * 10, F WITH i / 4, C WITH CHR(70 - i)
   i = i + 1
ENDDO
fld = "F"
e = "P_URIC_ACI + 1"
GO 2
REPLACE &fld WITH 9.75
REPLACE P_URIC_ACI WITH &e
REPLACE &fld WITH &e / 2, C WITH "zz"
? "replaced:", P_URIC_ACI, F, C
c = "P_URIC_ACI > 25"
COUNT FOR &c TO n1
COUNT FOR &c .AND. F < 1.5 TO n2
GO TOP
COUNT WHILE P_URIC_ACI < 50 .AND. &c TO n3
? "counted:", n1, n2, n3
SUM P_URIC_ACI, F TO s1, s2 FOR &c
AVERAGE F, p_URIC_ACID / 3 TO a1, a2 FOR RECNO() < 4
? "summed:", s1, s2, "averaged:", a1, a2
k = "C"
INDEX ON &k TO MCMDK
GO TOP
? "first by key:", C, RECNO()
SET FILTER TO &c
GO TOP
? "filtered first:", RECNO()
SET FILTER TO
? "ten characters:", p_URIC_ACID
CLOSE ALL
? "after CLOSE ALL:", TYPE("fld"), fld
? "MOD(7, 0):", MOD(7, 0)
ERASE MCMD.DBF
ERASE MCMDK.NDX
