* ACHOICE() where the guide and Harbour part: an unselectable initial
* item gives way to the first selectable one (the guide's rule; Harbour
* keeps the unselectable item), and the interpreter and the compiled
* program must agree on the rest of the keyless path too.  KEYBOARD is the
* keyboard here, so no terminal is needed.
a = {"Apple", "Banana", "Cherry", "Date"}
s = {.T., .T., .F., .T.}
KEYBOARD CHR(13)
? "initial 3 unselectable:", ACHOICE(5, 5, 8, 20, a, s, , 3)
KEYBOARD CHR(13)
? "initial 4:", ACHOICE(5, 5, 8, 20, a, s, , 4)
KEYBOARD CHR(24) + CHR(24) + CHR(13)
? "down twice over Cherry:", ACHOICE(5, 5, 8, 20, a, s)
KEYBOARD CHR(6) + CHR(5) + CHR(13)
? "end up over Cherry:", ACHOICE(5, 5, 8, 20, a, s)
? "none selectable:", ACHOICE(5, 5, 8, 20, a, .F.)
KEYBOARD "c" + CHR(13)
? "letter of an unselectable item:", ACHOICE(5, 5, 8, 20, a, s)
