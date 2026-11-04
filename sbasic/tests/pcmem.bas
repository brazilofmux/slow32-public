' the PC at its addresses, as QBasic 1.1 sees it in text mode: B800's
' cells (a character, then its attribute) POKEd and PEEKed and read back
' by SCREEN(), the BIOS's video mode, columns and rows, and memory below
' 64K keeping a POKE, the ports' ranges (a port, then a value or mask,
' each too large or small) and the DAC's six bits -- the values printed last
DIM r(30)
ON ERROR GOTO caught
CLS
PRINT "Hello"
DEF SEG = &HB800
r(0) = PEEK(0): r(1) = PEEK(1): r(2) = PEEK(8)
POKE 160, ASC("Q"): POKE 161, &H1E
POKE 162 + 2 * 79, ASC("z")
r(3) = SCREEN(2, 1): r(4) = SCREEN(2, 1, 1): r(5) = SCREEN(3, 80): r(6) = PEEK(161)
DEF SEG = &H40
r(7) = PEEK(&H49): r(8) = PEEK(&H4A): r(9) = PEEK(&H4B)
DEF SEG = 0
r(10) = PEEK(&H449): r(11) = PEEK(&H44A)
DEF SEG = &H1000
POKE 5, 77: r(12) = PEEK(5)
DEF SEG
n = 13
r(n) = PEEK(70000)
OUT &H3C8, 9: OUT &H3C9, 300: OUT &H3C9, -255: OUT &H3C9, 191
OUT &H3C7, 9: r(n) = INP(&H3C9): r(n + 1) = INP(&H3C9): r(n + 2) = INP(&H3C9): n = n + 3
r(n) = INP(-65536): r(n + 1) = INP(65535): n = n + 2
x = INP(-65537)
OUT 65536, 0
OUT -40000, 0
OUT &H3C8, 32768
OUT &H3C8, -32769
WAIT &H3DA, 32768
WAIT &H3DA, 8, -32769
WAIT 70000, 1
CLS
FOR i = 0 TO n - 1: PRINT r(i);: NEXT
END
caught:
r(n) = 1000 + ERR: n = n + 1
RESUME NEXT
