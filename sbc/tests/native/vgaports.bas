' the PC as a QBasic program sees it, compiled: the VGA DAC through
' ports 3C7/3C8/3C9, the mode-13 frame at A000, the BIOS data area's
' video mode and columns, the text screen at B800, WAIT on the retrace
' bit, and the errors (a value past a byte, a port past an integer).
' (WIDTH 80: QBasic's SCREEN 0 after 13 keeps 40 columns; ours does not yet)
DEFINT A-Z
DIM got(20), errs(5)
ON ERROR GOTO trapped
SCREEN 13
OUT &H3C8, 17: OUT &H3C9, 12: OUT &H3C9, 40: OUT &H3C9, 63
OUT &H3C9, 1: OUT &H3C9, 2: OUT &H3C9, 3
OUT &H3C7, 17
FOR k = 0 TO 5: got(k) = INP(&H3C9): NEXT
DEF SEG = &HA000
POKE 320 * 12 + 9, 17
got(6) = POINT(9, 12)
PSET (33, 4), 140
got(7) = PEEK(4 * 320 + 33)
DEF SEG = 0
got(8) = PEEK(&H449): got(9) = PEEK(&H44A)
WAIT &H3DA, 8
WAIT &H3DA, 8, 8
got(10) = INP(&H3DA) AND 246
OUT &H3C8, 300
OUT 70000, 1
SCREEN 0
WIDTH 80
DEF SEG = 0
got(11) = PEEK(&H449): got(12) = PEEK(&H44A)
DEF SEG = &HB800
POKE 160 * 2 + 4, 81
got(13) = SCREEN(3, 3)
got(14) = PEEK(160 * 2 + 4)
DEF SEG
LOCATE 5, 1
FOR k = 0 TO 14: PRINT got(k);: NEXT
PRINT
FOR k = 0 TO ne - 1: PRINT errs(k);: NEXT
PRINT
END
trapped:
errs(ne) = ERR: ne = ne + 1
RESUME NEXT
