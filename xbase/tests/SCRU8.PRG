* SAVESCREEN in UTF-8 (ruling 5): four bytes a cell -- the code point low byte
* first, its half in the third byte's top bits, the attribute -- read with the
* byte functions; a CJK character's second cell is its blank half
SET TALK OFF
SET CODEPAGE TO "UTF8"
CLEAR
@ 0,0 SAY "日é─"
s = SAVESCREEN(0, 0, 0, 4)
CLEAR
RESTSCREEN(0, 0, 0, 4, s)
t = SAVESCREEN(0, 0, 0, 4)
@ 2,0 SAY ""
? BLEN(s), s == t, BASC(s), BASC(BSUBSTR(s, 2)), BASC(BSUBSTR(s, 4)), BASC(BSUBSTR(s, 5))
