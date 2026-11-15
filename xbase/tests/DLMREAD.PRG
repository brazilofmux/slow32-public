* test_append_dbase3: each record, bracketed
? RECCOUNT()
GO TOP
DO WHILE .NOT. EOF()
   ? "[" + TRIM(DATA) + "]"
   SKIP
ENDDO
