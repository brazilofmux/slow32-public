PRINT "start"
OPEN "err-eof.bas" FOR INPUT AS #1
DO: LINE INPUT #1, A$: PRINT A$: LOOP
