' arrays through their cells, held to the interpreter
DIM A%(5), B&(2, 3), C(3), D#(2, 2, 2), E$(4)
FOR I = 0 TO 5: A%(I) = I * 100: NEXT
FOR I = 0 TO 2: FOR J = 0 TO 3: B&(I, J) = I * 100000 + J: NEXT: NEXT
FOR I = 0 TO 3: C(I) = I / 3: NEXT
FOR I = 0 TO 2: FOR J = 0 TO 2: FOR K = 0 TO 2: D#(I, J, K) = I + J / 10 + K / 100: NEXT: NEXT: NEXT
FOR I = 0 TO 4: E$(I) = STRING$(I, "x") + "y": NEXT
PRINT A%(5); B&(2, 3); C(1); D#(2, 1, 2); E$(3)
PRINT A%(2.5); A%(3.5); C(1.4)
S$ = "shared": E$(0) = S$: E$(1) = S$: E$(0) = "other": PRINT E$(0); E$(1); S$
E$(2) = E$(2) + E$(2): PRINT E$(2)
X(3) = 7: PRINT X(3); X(10); Y$(2); "|"
Z%(1, 2) = 5: PRINT Z%(1, 2) + Z%(10, 10)
ERASE A%: PRINT A%(1): A%(1) = 9: PRINT A%(1)
REDIM R(3): R(3) = 1: REDIM R(20): PRINT R(3); R(20)
FOR I = 1 TO 3: C(I) = C(I) * 3: NEXT: PRINT C(1); C(2); C(3)
T = 0: FOR I = 1 TO 1000: T = T + C(I MOD 3 + 1): NEXT: PRINT T
