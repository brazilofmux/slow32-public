* PARAMETER, PARA: PARAMETERS abbreviated, as dBASE takes it.  The
* parameters are the caller's variables, and the line after them runs
* (the singular form was dispatched as a command that stepped past it)
t = 1
DO P1 WITH 5, t
? "PARAMETER", t
t = 1
DO P2 WITH 5, t
? "PARA", t
t = 1
DO P3 WITH 5, t
? "PARAMETERS", t

PROCEDURE P1
PARAMETER old,new
new = old + 1
RETURN

PROCEDURE P2
PARA a, b
b = a + 2
RETURN

PROCEDURE P3
PARAMETERS a, b
b = a + 3
RETURN
