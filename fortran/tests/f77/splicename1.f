      PROGRAM SPLNM1
C     A spliced callee's names end with the splice: the caller's first
C     use of a name after the call makes the caller's own variable.
      INTEGER K, F, M
      DIMENSION W(3)
      M = 7
      K = F(3)
C     X is REAL here, INTEGER in F
      X = 2.5
      PRINT *, K, X
C     N is S's dummy, bound to M: the caller's N is its own
      CALL S(M)
      N = 99
      PRINT *, M, N
C     V is an array in G, a scalar here; Y is DOUBLE PRECISION there
      W(1) = 1.5
      W(2) = 2.5
      W(3) = 3.5
      R = G(W)
      V = 4.25
      Y = 1.125
      PRINT *, R, V, Y
C     and again, the names of a second splice of the same callee
      K = F(4) + F(5)
      J = K
      PRINT *, J
      END
      INTEGER FUNCTION F(J)
      INTEGER X
      X = J * 2
      F = X
      END
      SUBROUTINE S(N)
      N = N + 1
      END
      REAL FUNCTION G(V)
      DIMENSION V(3)
      DOUBLE PRECISION Y
      Y = V(1) + V(2) + V(3)
      G = Y
      END
