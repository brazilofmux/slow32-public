* SORT on a list of keys, each with its flags, and a FOR after them
* (SORT read the first key and dropped the rest, the FOR too); equal keys
* keep their order; a SORT that selects nothing still makes its table
x = DBCREATE("SRK", {{"P", "C", 1, 0}, {"L", "C", 5, 0}, {"N", "N", 3, 0}, {"T", "C", 1, 0}})
USE SRK
APPEND BLANK
REPLACE P WITH "2", L WITH "beta", N WITH 5, T WITH "a"
APPEND BLANK
REPLACE P WITH "1", L WITH "Alpha", N WITH 7, T WITH "b"
APPEND BLANK
REPLACE P WITH "2", L WITH "ALPHA", N WITH 9, T WITH "c"
APPEND BLANK
REPLACE P WITH "1", L WITH "alpha", N WITH 7, T WITH "d"
APPEND BLANK
REPLACE P WITH "2", L WITH "Beta", N WITH 1, T WITH "e"
SORT TO SRK2 ON P, L /C, N /D
USE SRK2
s = ""
DO WHILE .NOT. EOF()
   s = s + T
   SKIP
ENDDO
? "ON P, L /C, N /D:", s
USE SRK
SORT TO SRK3 ON P /D, N FOR N > 4
USE SRK3
s = ""
DO WHILE .NOT. EOF()
   s = s + T
   SKIP
ENDDO
? "ON P /D, N FOR N > 4:", s
USE SRK
SORT TO SRK4 ON L FOR N > 100
USE SRK4
? "FOR none:", RECCOUNT()
USE
ERASE SRK.DBF
ERASE SRK2.DBF
ERASE SRK3.DBF
ERASE SRK4.DBF
