' files in s32-bc: OPEN modes, PRINT # (USING), WRITE #, INPUT #, LINE INPUT #,
' EOF, GET/PUT/SEEK, LSET/RSET, NAME, KILL
OPEN "bcf1.tmp" FOR OUTPUT AS #1
PRINT #1, "alpha"; 12; -3.5
PRINT #1, USING "##.## &"; 2.345; "x"
WRITE #1, "q,uoted", 7, 1.25
PRINT #1, "tab"; TAB(10); "col";
PRINT #1, ""
CLOSE #1
OPEN "bcf1.tmp" FOR APPEND AS #2: PRINT #2, "appended": CLOSE #2
OPEN "bcf1.tmp" FOR INPUT AS #1
LINE INPUT #1, L$: PRINT "["; L$; "]"
LINE INPUT #1, L$: PRINT "["; L$; "]"
INPUT #1, A$, N%, D#: PRINT A$; N%; D#
DO WHILE NOT EOF(1)
    LINE INPUT #1, L$: PRINT "<"; L$; ">"
LOOP
CLOSE
OPEN "bcf2.tmp" FOR BINARY AS #3
I% = -2: L& = 70000: S! = 1.5: D# = 1 / 3: T$ = "bytes"
PUT #3, , I%: PUT #3, , L&: PUT #3, , S!: PUT #3, , D#: PUT #3, , T$
I% = 0: L& = 0: S! = 0: D# = 0: T$ = "....."
GET #3, 1, I%: GET #3, , L&: GET #3, , S!: GET #3, , D#: GET #3, , T$
PRINT I%; L&; S!; D#; T$
SEEK #3, 3: GET #3, , L&: PRINT L&; LOF(3); LOC(3)
CLOSE #3
OPEN "bcf3.tmp" FOR RANDOM AS #4 LEN = 8
R$ = SPACE$(8): LSET R$ = "left": PUT #4, 2, R$: RSET R$ = "right": PUT #4, 1, R$
R$ = SPACE$(8): GET #4, 2, R$: PRINT "["; R$; "]": GET #4, 1, R$: PRINT "["; R$; "]"
E$ = "": LSET E$ = "nothing": PRINT "["; E$; "]"
CLOSE #4
NAME "bcf3.tmp" AS "bcf4.tmp"
OPEN "bcf4.tmp" FOR INPUT AS #1: PRINT LOF(1): CLOSE #1
KILL "bcf1.tmp": KILL "bcf2.tmp": KILL "bcf4.tmp"
PRINT "done"
