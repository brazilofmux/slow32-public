' DIM a(lo TO hi) in s32-bc: per-dimension lower bounds, static and not
DIM B(-3 TO 3), C%(1 TO 2, 10 TO 12), D#(0 TO 1, -1 TO 1, 5 TO 6)
FOR I = -3 TO 3: B(I) = I * I: NEXT: PRINT B(-3); B(0); B(3)
C%(2, 12) = 5: C%(1, 10) = 7: PRINT C%(2, 12) + C%(1, 10); LBOUND(C%, 2); UBOUND(C%, 1)
D#(1, -1, 6) = 2.5: PRINT D#(1, -1, 6); LBOUND(D#, 3); UBOUND(D#, 3)
N = 4: REDIM E$(N TO N + 2): E$(6) = "six": PRINT E$(6); LBOUND(E$); UBOUND(E$)
REDIM E$(-N TO 0): E$(-4) = "m": PRINT E$(-4); UBOUND(E$)
CALL Inner
END
SUB Inner
    DIM L(5 TO 7)
    L(5) = 1: L(7) = 3: PRINT L(5) + L(7); LBOUND(L)
END SUB
