* @ ... GET and READ in line mode: each GET takes a line of the keys
* the harness types (kbdget.keys); pictures, VALID and an empty line.
LOCAL cCode := SPACE(5), nQty := 0, dWhen := CTOD(""), lOk := .F., cKeep := "keep "
SET DATE ANSI
SET CENTURY ON
@ 1, 1 SAY "Code" GET cCode PICTURE "@!"
@ 2, 1 SAY "Qty " GET nQty PICTURE "999" VALID nQty > 0
@ 3, 1 SAY "When" GET dWhen
@ 4, 1 SAY "OK? " GET lOk PICTURE "Y"
@ 5, 1 SAY "Keep" GET cKeep
READ
? "[" + cCode + "]", nQty, DTOC(dWhen), lOk, "[" + cKeep + "]"
? LASTKEY() == 13, UPDATED()
RETURN
