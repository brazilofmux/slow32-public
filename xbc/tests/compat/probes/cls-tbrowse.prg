* Classes: TBrowse and TBColumn -- the navigation blocks, the columns,
* movement and hitTop / hitBottom, rowCount from the window
STATIC a := {{"ann", 1}, {"bob", 2}, {"cy", 3}}, n := 1
PROCEDURE Main()
LOCAL oB, oC
oB := TBrowseNew(2, 0, 8, 30)
oB:goTopBlock := {|| n := 1}
oB:goBottomBlock := {|| n := LEN(a)}
oB:skipBlock := {|k| Skipper(k)}
oC := TBColumnNew("Name", {|| a[n][1]})
oB:addColumn(oC)
oB:addColumn(TBColumnNew("Num", {|| a[n][2]}))
? oB:className(), oB:colCount, oB:rowCount, oB:nTop, oB:nLeft, oB:nBottom, oB:nRight
oB:down()
oB:stabilize()
? "after", oB:rowPos, oB:colPos, n, oB:hitBottom
oB:goBottom()
DO WHILE !oB:stabilize()
ENDDO
? "bottom", oB:rowPos, n, oB:hitBottom
oB:down()
DO WHILE !oB:stabilize()
ENDDO
? "past", oB:rowPos, n, oB:hitBottom
oB:up()
oB:up()
oB:up()
DO WHILE !oB:stabilize()
ENDDO
? "top", oB:rowPos, n, oB:hitTop
oB:right()
? "col", oB:colPos
oC:heading := "Who"
? oB:getColumn(1):heading, oC:width, oB:getColumn(2):width, "[" + oB:colSep + "]", "[" + oB:headSep + "]"
RETURN
FUNCTION Skipper(k)
LOCAL s := IF(k > 0, MIN(k, LEN(a) - n), MAX(k, 1 - n))
n += s
RETURN s
