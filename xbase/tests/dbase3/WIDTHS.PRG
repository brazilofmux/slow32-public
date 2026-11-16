* dBASE III PLUS's own display, which the interpreter is held to: the
* widths it gives numbers (literals, fields, variables, + - * /, the
* functions), SET DECIMALS, a zero divisor, LIST and DISPLAY, and the
* TALK lines.  dBASE III PLUS 1.1 ran this on WIDTHS.DBF to make WIDTHS.TXT.
SET TALK OFF
SET SAFETY OFF
USE WIDTHS
APPEND BLANK
REPLACE A WITH 123, F WITH 45.5, G WITH 7.125, B WITH 9, H WITH 1234567, K WITH 12.5, M WITH 1.25, C WITH "ab", L WITH .T.
APPEND BLANK
REPLACE A WITH 7, F WITH -1.25, G WITH 2, B WITH 31, H WITH 5, K WITH 0.5, M WITH 9.99, C WITH "cd"
GO TOP
x = A + 1
y = 5
r = 12345678901
r = r + 1
p = H * H
j = 0
DO WHILE j < 20
   j = j + 1
ENDDO
n = 0
SET ALTERNATE TO WOUT
SET ALTERNATE ON
? 5
? 123
? 1234567890
? 5.5
? 5.25
? -5
? 0
? 0.5
? .5
? 1.
? -0.5
? A
? F
? G
? B
? H
? K
? M
? A + 1
? A + F
? A * 2
? A * F
? F * F
? A - B
? A / 4
? 24 / 4
? 10 / 4
? 1 / 3
? A / B
? F / 2
? G / F
? F / G
? H / A
? M / K
? 100 / 0.001
? -A
? (A + 1) * 2
? A + A + A + A
? B * B * B
? x
? y
? r
? p
? j
? x + 1
? y * y
? INT(F)
? INT(G)
? INT(A / 7)
? ROUND(F, 1)
? ROUND(G, 2)
? ROUND(A, -1)
? ROUND(-2.5, 0)
? ROUND(-1.25, 1)
? ABS(-F)
? MOD(A, B)
? MOD(F, B)
? MOD(-A, 7)
? MOD(5, n)
? SQRT(A)
? LOG(A)
? EXP(1)
? MAX(F, G)
? MIN(A, F)
? VAL("12.5")
? VAL("7")
? STR(A)
? STR(123.456)
? STR(-0.4)
? LEN(STR(F))
? RECNO()
? DOW(CTOD("01/15/86"))
? 1 / n
? IIF(1 / n > 0, "gt", "not gt")
? IIF(0 / n = 0, "eq", "not eq")
? -1 / 300
? TRANSFORM(A, "")
? A, F, x
SET DECIMALS TO 0
? 1 / 3
? -1 / 3
? A / 4
SET DECIMALS TO 4
? 1 / 3
? G / 2
? VAL("12.5")
SET DECIMALS TO 2
LIST
LIST OFF
LIST A, C, L
LIST A * F, C + "!", -K
LIST OFF A + 1, UPPER(C)
GO 1
DISPLAY A, F
DISPLAY OFF C
GO BOTTOM
SKIP
DISPLAY A
SET TALK ON
z = A + 1
STORE 2.5 TO w
GO 1
SKIP
GO 2
REPLACE B WITH B + 1
REPLACE ALL B WITH B + 1
LOCATE FOR A = 7
CONTINUE
DELETE
RECALL
COUNT TO c
SUM A, F TO s1, s2
AVERAGE A, F TO a1, a2
INDEX ON C TO WIDTHSC
SEEK "zz"
SET TALK OFF
CLOSE ALTERNATE
USE
ERASE WIDTHSC.NDX
QUIT
