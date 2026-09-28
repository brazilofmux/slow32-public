identification division.
program-id. usingfirst.
*> A USING program whose prologue makes calls of its own: IS INITIAL
*> (the CANCEL routine) and DECIMAL-POINT IS COMMA.  The addresses in
*> the argument registers used to be stored after those calls had
*> clobbered them, and the program wrote to address 1.
data division.
working-storage section.
01  a pic x(5) value "hello".
01  b pic 9(3) value 42.
procedure division.
m1.
    call "usingsub" using a b
    display "main: " a " " b
    stop run.
end program usingfirst.
identification division.
program-id. usingsub is initial.
environment division.
configuration section.
special-names.
    decimal-point is comma.
data division.
linkage section.
01  x pic x(5).
01  y pic 9(3).
procedure division using x y.
s1.
    display "sub: " x " " y
    move "world" to x
    add 1 to y
    exit program.
end program usingsub.
