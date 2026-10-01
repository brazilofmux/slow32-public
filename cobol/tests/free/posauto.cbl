*> A positioned ACCEPT with the screen clauses written on the statement,
*> as Micro Focus and RM/COBOL allow (BP-E7): WITH UPDATE AUTO-SKIP --
*> abrignoli_COBSOFT's form, 24 programs -- ends the field when it is
*> full, so the next field's keys follow with no Enter between.  It was
*> "'auto-skip' is not a COBOL verb" (ISSUES 120).  The keys come from
*> posauto.keys; the ANSI stream is the expected output.
*> No oracle: GnuCOBOL's screens need a real tty.
identification division.
program-id. posauto.
data division.
working-storage section.
01 cd-pais pic 99 value 7.
01 nome    pic x(3).
01 result.
   05 filler pic x(5) value "pais=".
   05 r-pais pic 99.
   05 filler pic x(6) value " nome=".
   05 r-nome pic x(3).
procedure division.
    accept cd-pais at line 2 col 1 with update auto-skip
    accept nome at line 3 col 1 with auto-skip underline
    move cd-pais to r-pais move nome to r-nome
    display result at line 5 col 1
    stop run.
