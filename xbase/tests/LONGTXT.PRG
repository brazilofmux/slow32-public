* A value's text past 255 bytes (it lives in ext): functions that change
* text make a new value of all of it (xbase ISSUES, value_t step 1)
SET TALK OFF
s = REPLICATE("ab", 150) + CHR(141) + CHR(10) + "cd" + CHR(13) + CHR(10) + "ef"
? LEN(s)
h = HARDCR(s)
? LEN(h), ASC(SUBSTR(h, 301, 1)), ASC(SUBSTR(h, 305, 1))
m = MEMOTRAN(s)
? LEN(m), RIGHT(m, 7)
d = DESCEND(s)
? LEN(d), ASC(RIGHT(d, 1)), ASC(LEFT(d, 1))
? DESCEND(d) == s
? HARDCR("x" + CHR(141) + CHR(10) + "y") == "x" + CHR(13) + CHR(10) + "y"
