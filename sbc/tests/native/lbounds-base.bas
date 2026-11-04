' OPTION BASE where no lower bound is written, in s32-bc
OPTION BASE 1
DIM F(3), G(0 TO 2): F(1) = 1: G(0) = 2: PRINT F(1); G(0); LBOUND(F); LBOUND(G)
N = 2: DIM H(N), J(-1 TO N): PRINT LBOUND(H); UBOUND(H); LBOUND(J)
