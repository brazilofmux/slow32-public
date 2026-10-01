*> Reference modification in screen I/O: a positioned ACCEPT into a part
*> of an item, a positioned DISPLAY of a part, and a SCREEN SECTION field
*> FROM a part.  abrignoli_COBSOFT
*> keys a CPF number piece by piece, accept f-cpf(07:03) at line 11 col
*> 42 with update auto-skip.  Each was "not implemented" (ISSUES 120).
*> The keys come from posrefmod.keys; the ANSI stream is the expected
*> output.  No oracle: GnuCOBOL's screens need a real tty.
identification division.
program-id. posrefmod.
data division.
working-storage section.
01 f-cpf pic x(11) value all "0".
screen section.
01 show-mid.
   05 line 4 col 1 value "middle:".
   05 line 4 col 9 pic x(3) from f-cpf(4:3).
procedure division.
    accept f-cpf(1:3) at line 2 col 1 with auto-skip
    accept f-cpf(4:3) at line 2 col 5 with auto-skip
    accept f-cpf(7:3) at line 2 col 9 with auto-skip
    display show-mid
    display f-cpf at line 6 col 1
    display f-cpf(10:2) at line 7 col 1
    stop run.
