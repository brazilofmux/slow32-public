* Functions: SAVESCREEN()'s string is the cells themselves, a character
* and its colour byte each (GR+/B is 1E); RESTSCREEN() lays a string
* row by row over its region, as far as the string goes; SCROLL() moves
* a region's rows up or down, the rows left behind blank, and with no
* count clears it.  The rows are put there by RESTSCREEN.
LOCAL cRows := "a" + CHR(7) + "b" + CHR(30) + "c" + CHR(7) + "d" + CHR(7)
RESTSCREEN(2, 0, 5, 0, cRows)
? "rows", Hex(SAVESCREEN(2, 0, 5, 0)), LEN(SAVESCREEN(2, 0, 5, 3))
SCROLL(2, 0, 5, 0, 1)
? "up", Hex(SAVESCREEN(2, 0, 5, 0))
SCROLL(2, 0, 5, 0, -2)
? "down", Hex(SAVESCREEN(2, 0, 5, 0))
RESTSCREEN(2, 0, 5, 0, cRows)
SCROLL(2, 0, 5, 0, 9)
? "past", Hex(SAVESCREEN(2, 0, 5, 0))
RESTSCREEN(2, 0, 5, 0, cRows)
SCROLL(3, 0, 4, 0)
? "clear", Hex(SAVESCREEN(2, 0, 5, 0))
RESTSCREEN(7, 0, 8, 2, "xy" + CHR(9) + "z")
? "part", Hex(SAVESCREEN(7, 0, 7, 1))
RETURN

STATIC FUNCTION Hex(s)
   LOCAL i, r := ""
   FOR i := 1 TO LEN(s)
      r += SUBSTR("0123456789ABCDEF", INT(ASC(SUBSTR(s, i, 1)) / 16) + 1, 1) + ;
           SUBSTR("0123456789ABCDEF", ASC(SUBSTR(s, i, 1)) % 16 + 1, 1)
   NEXT
RETURN r
