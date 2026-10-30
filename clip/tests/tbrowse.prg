* A TBrowse painted in line mode over an array; @ GET into GetList
STATIC a := {{"ann", 1.5}, {"bob", 22}, {"cy", 3}}, n := 1
LOCAL oB, GetList := {}, cName := "x", i
oB := TBrowseNew(1, 0, 6, 25)
oB:goTopBlock := {|| n := 1}
oB:goBottomBlock := {|| n := LEN(a)}
oB:skipBlock := {|k| Skipper(k)}
oB:addColumn(TBColumnNew("Name", {|| a[n][1]}))
oB:addColumn(TBColumnNew("Amount", {|| a[n][2]}))
oB:headSep := "-"
oB:colSep := " | "
oB:down()
oB:stabilize()
? "row", oB:rowPos, "rec", n
oB:goBottom()
oB:stabilize()
? "row", oB:rowPos, "rec", n
oB:pageUp()
oB:stabilize()
? "row", oB:rowPos, "rec", n, oB:hitTop
@ 10, 2 GET cName
? LEN(GetList), GetList[1]:name, GetList[1]:row, GetList[1]:col
CLEAR GETS
RETURN
FUNCTION Skipper(k)
LOCAL s := IF(k > 0, MIN(k, LEN(a) - n), MAX(k, 1 - n))
n += s
RETURN s
