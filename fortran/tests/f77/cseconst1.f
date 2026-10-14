C     ** CSE takes two integer constants of one value as one operand,
C     within a block: strength reduction gives each 2*I its own
C     constant 1 for the shift, and the two copies of 2*I-1 in
C     MOD(X,2*I-1) and X/(2*I-1) did not meet.  The spigot's inner
C     loop, run backwards over a short array, and MOD and divide of
C     either sign, with a call, another divide and a label between
C     the two.  From fort370.  Self-checking; success reaches STOP 0.
      INTEGER A(50), I, J, Q, X, ISUM
      INTEGER XS(4), YS(4), R, R2, Q2, R3, Q3, R4, Q4, K, IT
      DATA XS /47, -47, 47, -47/, YS /5, 5, -5, -5/
      DO 10 I = 1, 50
         A(I) = 2
10    CONTINUE
      ISUM = 0
      DO 30 J = 1, 12
         Q = 0
         DO 20 I = 50, 1, -1
            X = 10 * A(I) + Q * I
            A(I) = MOD (X, 2 * I - 1)
            Q = X / (2 * I - 1)
20       CONTINUE
         A(1) = MOD (Q, 10)
         ISUM = ISUM * 10 + Q / 10
         IF (ISUM .GT. 100000) ISUM = MOD (ISUM, 100000)
30    CONTINUE
      WRITE (6, 100) ISUM
100   FORMAT (1X, '>SPIGOT', I8)
      K = 0
      IT = 0
      DO 50 I = 1, 4
         R = MOD (XS(I), YS(I))
         Q = XS(I) / YS(I)
         R2 = MOD (XS(I), YS(I) + 1)
         CALL BUMP (K)
         Q2 = XS(I) / (YS(I) + 1)
         R3 = MOD (XS(I), YS(I) - 1)
         K = K + XS(I) / 3
         Q3 = XS(I) / (YS(I) - 1)
         R4 = MOD (XS(I) + 1, YS(I))
         IF (K .LT. -1000) GO TO 45
45       Q4 = (XS(I) + 1) / YS(I)
         WRITE (6, 101) XS(I), YS(I), Q, R, Q2, R2, Q3, R3, Q4, R4
         IT = IT * 3 + Q + R + Q2 + R2 + Q3 + R3 + Q4 + R4
50    CONTINUE
101   FORMAT (1X, '>DIVREM', 2I4, 8I4)
      WRITE (6, 102) K, IT
102   FORMAT (1X, '>DIVREM K', I6, I8)
      IF (ISUM .NE. 65358) STOP 1
      IF (K .NE. 4000) STOP 2
      IF (IT .NE. 856) STOP 3
      STOP 0
      END
      SUBROUTINE BUMP (K)
      INTEGER K
      K = K + 1000
      END
