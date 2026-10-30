* Commands: REPORT FORM in CA-Clipper's layout -- the page header, title
* lines, column headings (numeric ones right-aligned), groups and
* subgroups with their subtotals, wrapped character columns, a page break,
* PLAIN, SUMMARY, NOEJECT, HEADING and FOR.  The .frm is written here.
LOCAL aCols := { {"NAME", "Name", 5, 0, .F.}, {"CITY", "City;of birth", 8, 0, .F.}, ;
                 {"AGE", "Age", 6, 1, .T.}, {"JOINED", "Joined", 8, 0, .F.}, {"ACTIVE", "Act", 3, 0, .F.} }
DBCREATE("crf_p", {{"NAME", "C", 15, 0}, {"CITY", "C", 12, 0}, {"AGE", "N", 3, 0}, {"JOINED", "D", 8, 0}, {"ACTIVE", "L", 1, 0}})
USE crf_p
AddRow("Ada", "London", 36, "01/05/1990", .T.)
AddRow("Grace", "New York", 45, "12/09/1991", .T.)
AddRow("Linus", "Helsinki", 22, "03/03/1992", .F.)
AddRow("Ken", "Murray Hill", 50, "07/07/1993", .T.)
AddRow("Dennis", "Murray Hill", 51, "08/08/1993", .T.)
GO TOP
MakeFrm("crf_g.frm", "Staff Listing;by city", "ACTIVE", "CITY", "Active", "City", aCols, 16)
MakeFrm("crf_s.frm", "", "", "", "", "", ;
        { {"NAME", "Name", 15, 0, .F.}, {"AGE", "Age", 3, 0, .T.} }, 58)
? "<grouped>"
REPORT FORM crf_g
? "<summary>"
REPORT FORM crf_g SUMMARY
? "<simple noeject>"
REPORT FORM crf_s NOEJECT
? "<plain>"
REPORT FORM crf_s PLAIN
? "<heading for>"
REPORT FORM crf_s HEADING "Staff" FOR AGE > 40
? "<end>"
USE
FERASE("crf_p.dbf")
FERASE("crf_g.frm")
FERASE("crf_s.frm")

STATIC FUNCTION AddRow(cName, cCity, nAge, cDate, lActive)
APPEND BLANK
REPLACE NAME WITH cName, CITY WITH cCity, AGE WITH nAge, JOINED WITH CTOD(cDate), ACTIVE WITH lActive
RETURN NIL

* The .frm layout: 1990 bytes, 2 at both ends; expression lengths at 4,
* offsets at 114, the text from 224; column slots of 12 from 1676; the
* parameters from 1964
STATIC FUNCTION MakeFrm(cFile, cTitle, cGrp, cSub, cGHdr, cSHdr, aCols, nLines)
LOCAL aExpr := {}, cLens := "", cOffs := "", cText := "", cCols := "", nPos := 0, i, h, aP, c
aP := { Ex(aExpr, cTitle), Ex(aExpr, cGrp), Ex(aExpr, cSub), Ex(aExpr, cGHdr), Ex(aExpr, cSHdr) }
FOR i := 1 TO LEN(aCols)
   AADD(aCols[i], Ex(aExpr, aCols[i][1]))
   AADD(aCols[i], Ex(aExpr, aCols[i][2]))
NEXT
FOR i := 1 TO LEN(aExpr)
   cLens += I2BIN(LEN(aExpr[i]) + 1)
   cOffs += I2BIN(nPos)
   cText += aExpr[i] + CHR(0)
   nPos += LEN(aExpr[i]) + 1
NEXT
FOR i := 1 TO LEN(aCols)
   c := aCols[i]
   cCols += I2BIN(c[3]) + REPLICATE(CHR(0), 3) + IIF(c[5], "Y", "N") + I2BIN(c[4]) + I2BIN(c[6]) + I2BIN(c[7])
NEXT
h := FCREATE(cFile)
FWRITE(h, I2BIN(2) + I2BIN(nPos) + PADR(cLens, 110, CHR(0)) + PADR(cOffs, 110, CHR(0)) + PADR(cText, 1440, CHR(0)))
FWRITE(h, REPLICATE(CHR(0), 12) + PADR(cCols, 288, CHR(0)))
FWRITE(h, I2BIN(aP[1]) + I2BIN(aP[2]) + I2BIN(aP[3]) + I2BIN(aP[4]) + I2BIN(aP[5]) + ;
          I2BIN(80) + I2BIN(nLines) + I2BIN(8) + I2BIN(0) + I2BIN(LEN(aCols)) + "NNN" + CHR(0) + I2BIN(2))
FCLOSE(h)
RETURN NIL

STATIC FUNCTION Ex(aExpr, cText)
IF EMPTY(cText)
   RETURN 65535
ENDIF
AADD(aExpr, cText)
RETURN LEN(aExpr) - 1
