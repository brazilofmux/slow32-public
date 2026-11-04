' CHR$(0) in strings, as QBasic takes it, a byte like any other: compared,
' searched, cased, trimmed, cut, assigned into, repeated -- its bytes and
' lengths printed, since on the screen it is a blank
DECLARE SUB show (t$)
z$ = CHR$(0)
a$ = "x" + z$ + "b": b$ = "x" + z$ + "a": c$ = "x"
PRINT a$ = b$; a$ > b$; c$ < a$; c$ = a$; "x" = "x" + z$; z$ = ""; z$ > ""
PRINT INSTR(a$, "b"); INSTR(a$, z$); INSTR(2, a$, z$ + "b"); INSTR(z$ + "q", "q"); INSTR(a$, "")
show UCASE$("a" + z$ + "b")
show LCASE$("A" + z$ + "B")
show LTRIM$(" " + z$ + " a")
show RTRIM$("a " + z$ + " ")
show LEFT$(a$, 2)
show RIGHT$(a$, 2)
show MID$(a$, 2, 1)
show STRING$(2, z$)
show STRING$(2, 0)
show z$ + z$
w$ = "abc": MID$(w$, 2, 1) = z$: show w$
PRINT VAL("1" + z$ + "2"); ASC(z$ + "q"); LEN(z$)
PRINT USING "&|"; a$
PRINT USING "!|"; z$
END
SUB show (t$)
PRINT LEN(t$);
FOR i = 1 TO LEN(t$): PRINT ASC(MID$(t$, i, 1));: NEXT
PRINT
END SUB
