* Commands: LABEL FORM in CA-Clipper's layout -- two across with a gap
* and a margin, lines between, a line whose value is blank dropped and the
* label closed up, a line with no expression kept, band lines trimmed, a
* last partial band, and FOR.  The .lbl is written here.
DBCREATE("clf_p", {{"NAME", "C", 15, 0}, {"CO", "C", 10, 0}, {"CITY", "C", 12, 0}, {"AGE", "N", 3, 0}})
USE clf_p
AddRow("Ada", "", "London", 36)
AddRow("Grace", "Navy", "New York", 45)
AddRow("Linus", "", "Helsinki", 22)
AddRow("Ken", "Bell Labs", "Murray Hill", 50)
AddRow("Dennis", "Bell Labs", "Murray Hill", 51)
GO TOP
MakeLbl("clf_a.lbl", 6, 18, 2, 1, 3, 2, {"NAME", "CO", "", "TRIM(CITY) + ', ' + LTRIM(STR(AGE))", "''"})
MakeLbl("clf_b.lbl", 3, 12, 0, 0, 0, 1, {"NAME", "CITY"})
? "<two across>"
LABEL FORM clf_a
? "<one across>"
LABEL FORM clf_b
? "<for>"
LABEL FORM clf_a FOR AGE > 40
? "<end>"
USE
FERASE("clf_p.dbf")
FERASE("clf_a.lbl")
FERASE("clf_b.lbl")

STATIC FUNCTION AddRow(cName, cCo, cCity, nAge)
APPEND BLANK
REPLACE NAME WITH cName, CO WITH cCo, CITY WITH cCity, AGE WITH nAge
RETURN NIL

* The .lbl layout: 1034 bytes, 2 at both ends; the remark, then height,
* width, margin, lines between, spaces between and across as words; then
* sixteen 60-character lines
STATIC FUNCTION MakeLbl(cFile, nHeight, nWidth, nMargin, nLines, nSpaces, nAcross, aLines)
LOCAL cBody := "", i, h
FOR i := 1 TO 16
   cBody += PADR(IIF(i <= LEN(aLines), aLines[i], ""), 60)
NEXT
h := FCREATE(cFile)
FWRITE(h, CHR(2) + PADR("probe", 60) + I2BIN(nHeight) + I2BIN(nWidth) + I2BIN(nMargin) + ;
          I2BIN(nLines) + I2BIN(nSpaces) + I2BIN(nAcross) + cBody + CHR(2))
FCLOSE(h)
RETURN NIL
