* Character GETs: a template longer than the string edits only the string
* ("!A" over SPACE(1) is one column, the GET left when it is typed), and
* a string keeps its width (SPACE(3) accepted as it stood is three blanks)
c = SPACE(1)
d = SPACE(1)
e = SPACE(3)
@ 1,1 GET c PICTURE "!A"
@ 2,1 GET d
@ 3,1 GET e
READ
? c + d + "[" + e + "]"
QUIT
