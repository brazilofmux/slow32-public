* Keyboard input in line mode: ACCEPT, INPUT, WAIT, read from the keys
* the harness types (kbdaccept.keys).
ACCEPT "Name: " TO cName
? "Hello, " + cName
INPUT "A number: " TO nNum
? nNum * 2
INPUT "An expression: " TO xVal
? xVal, TYPE("xVal")
WAIT "Press a key: " TO cKey
? "[" + cKey + "]"
ACCEPT TO cEmpty
? LEN(cEmpty)
RETURN
