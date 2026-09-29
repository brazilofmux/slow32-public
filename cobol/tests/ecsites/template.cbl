*> The template for the EC-DATA-INCOMPATIBLE sites gate (run-tests.sh,
*> gate 6): @STMT@ is replaced by each line of sites.txt in turn.  n is a
*> numeric DISPLAY item whose content ("1a3") fails the NUMERIC class
*> test, bb a boolean DISPLAY item whose content ("1a0") fails the
*> BOOLEAN one; 2023 14.6.13.2 rules 1 and 2 say every statement that
*> references either as a sending item raises the condition while it is
*> checked.  The condition
*> is fatal, so each site is a program of its own.
identification division.
program-id. ecsite.
data division.
working-storage section.
01 raw  pic x(3) value "1a3".
01 n    redefines raw pic 9(3).
01 m    pic 9(3) value 2.
01 rawb pic x(3) value "1a0".
01 bb   redefines rawb pic 1(3).
01 bc   pic 1(3) value b"101".
01 bu   pic 1(3) usage bit.
01 x    pic x(20).
01 t.
   05 e pic x occurs 5.
01 k    pic 9(4) comp.
01 ix   usage index.
procedure division.
declaratives.
dx section.
    use after exception condition ec-data-incompatible.
d1.
    display "RAISED".
end declaratives.
main section.
m1.
>>TURN EC-DATA-INCOMPATIBLE CHECKING ON
    @STMT@
    display "not raised"
    stop run.
p2.
    display "p2"
    stop run.
identification division.
program-id. cv.
data division.
linkage section.
01 v binary-long.
procedure division using by value v.
    goback.
end program cv.
end program ecsite.
