REM EOF is true once the next read would find nothing: the loop reads
REM both lines (QBasic 1.1: <a> <b> end-1)
OPEN "eofloop.tmp" FOR OUTPUT AS #1: PRINT #1, "a": PRINT #1, "b": CLOSE #1
OPEN "eofloop.tmp" FOR INPUT AS #1
DO WHILE NOT EOF(1): LINE INPUT #1, L$: PRINT "<"; L$; ">": LOOP
PRINT "end"; EOF(1)
CLOSE #1: KILL "eofloop.tmp"
