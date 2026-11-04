REM DIM a(lo TO hi): each dimension its own lower bound; LBOUND and
REM UBOUND per dimension (checked against QBasic 1.1)
DIM B(-3 TO 3), C%(1 TO 2, 10 TO 12)
B(-3) = 1: C%(2, 12) = 5
PRINT B(-3); C%(2, 12); LBOUND(B); UBOUND(B); LBOUND(C%, 2); UBOUND(C%, 2)
N = 4: REDIM D(N TO N + 2): D(6) = 9: PRINT D(6); LBOUND(D)
DIM A(5 TO 5): A(5) = 2: PRINT A(5)
