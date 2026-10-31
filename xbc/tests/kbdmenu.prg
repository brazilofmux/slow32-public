* Menus, the keyboard buffer and a READ on a field, from the keys the
* harness types (kbdmenu.keys).
LOCAL nChoice := 1, nKey
@ 1, 1 PROMPT "Add"
@ 2, 1 PROMPT "Change"
@ 3, 1 PROMPT "Quit"
MENU TO nChoice
? "chose", nChoice
KEYBOARD "xy"
nKey := INKEY()
? nKey, CHR(INKEY()), INKEY()
CREATE kbdtmp
APPEND BLANK
REPLACE field_name WITH "NAME", field_type WITH "C", field_len WITH 10
USE
CREATE kbdfile FROM kbdtmp
USE kbdfile
APPEND BLANK
@ 5, 1 SAY "Name" GET name PICTURE "@!"
READ
? "[" + name + "]", RECCOUNT()
USE
ERASE kbdtmp.dbf
ERASE kbdfile.dbf
RETURN
