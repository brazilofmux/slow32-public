* Under an index, SCAN and COPY TO walk the index's order, as dBASE does
* (they walked record numbers: Teacher's Pet's grade report, beside its
* Quicksilver build, printed its grades out of order)
x = DBCREATE("SCI", {{"N", "C", 1, 0}})
USE SCI
APPEND BLANK
REPLACE N WITH "c"
APPEND BLANK
REPLACE N WITH "a"
APPEND BLANK
REPLACE N WITH "d"
APPEND BLANK
REPLACE N WITH "b"
INDEX ON N TO SCIN
s = ""
SCAN
   s = s + N
ENDSCAN
? "SCAN:", s
SEEK "b"
s = ""
SCAN REST
   s = s + N
ENDSCAN
? "SCAN REST from b:", s
SEEK "a"
s = ""
SCAN NEXT 2
   s = s + N
ENDSCAN
? "SCAN NEXT 2 from a:", s
COPY TO SCI2
COPY TO SCI3 SDF
USE SCI2
s = ""
DO WHILE .NOT. EOF()
   s = s + N
   SKIP
ENDDO
? "COPY TO:", s
USE
h = FOPEN("SCI3.TXT")
? "COPY TO SDF:", STRTRAN(STRTRAN(FREAD(h, 20), CHR(13), ""), CHR(10), "")
x = FCLOSE(h)
ERASE SCI.DBF
ERASE SCI2.DBF
ERASE SCI3.TXT
ERASE SCIN.NDX
