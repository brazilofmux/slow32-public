* Classes: the Get object -- GetNew, its variables, the buffer edited by
* method, assign and undo, the READ functions outside a READ
LOCAL GetList := {}, oGet, cName := "Ann  ", nAmt := 12.5
oGet := GetNew(3, 5, {|x| IF(x == NIL, cName, cName := x)}, "cName", "@!", "W/B")
? oGet:className(), oGet:row, oGet:col, oGet:name, oGet:picture, oGet:type
? oGet:varGet()
oGet:varPut("Bob")
? cName, oGet:changed, oGet:hasFocus, oGet:exitState
oGet:setFocus()
? oGet:buffer, oGet:hasFocus, oGet:pos
oGet:overstrike("z")
? oGet:buffer, oGet:changed, oGet:pos
oGet:assign()
? cName
oGet:undo()
? cName, oGet:original
oGet:killFocus()
AADD(GetList, GetNew(5, 1, {|x| IF(x == NIL, nAmt, nAmt := x)}, "nAmt", "999.99"))
? LEN(GetList), GetList[1]:name, GetList[1]:picture, GetList[1]:varGet()
GetList[1]:setFocus()
? "[" + GetList[1]:buffer + "]", GetList[1]:type
? ReadVar(), Updated(), LastKey()
