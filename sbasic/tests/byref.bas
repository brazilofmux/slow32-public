REM Parameters: variables and array elements by reference, (X) and
REM expressions by value, aliasing, a type mismatch (QBasic 1.1's output)
A = 1: CALL S(A): PRINT A
B = 1: CALL S((B)): PRINT B
DIM C(3): C(2) = 1: CALL S(C(2)): PRINT C(2)
D = 1: CALL S(D + 0): PRINT D
E = 1: X = F(E): PRINT E; X
G = 1: CALL T(G, G): PRINT G
I% = 1: CALL S(I%): PRINT I%
END
SUB S (P)
P = P + 10
END SUB
FUNCTION F (Q)
Q = Q * 3: F = Q
END FUNCTION
SUB T (P, Q)
P = P + 10: Q = Q + 100
END SUB
