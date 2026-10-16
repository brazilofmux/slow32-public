C     ** Constant expressions.  Complex arithmetic took the real parts
C     alone: (1.0,2.0)*(3.0,4.0) gave (3,4), not (-5,10), and a unary
C     minus left the imaginary part's sign.  ** was not there at all:
C     PARAMETER (N = 2**10) said "expected ) after PARAMETER list".  It
C     is right-associative, -2**2 is -(2**2), and it serves in array
C     bounds too.  And REAL*8 E1 was refused: squeezed, the scanner read
C     8E1 as a constant.  From fort370, which found all but the minus.
C     Self-checking; success reaches STOP 0.
      PROGRAM CPAR1
      COMPLEX P, Q, R, S, T, U
      INTEGER N, M, K, L
      REAL X, Y
      DOUBLE PRECISION D
      PARAMETER (P = (1.0, 2.0) * (3.0, 4.0))
      PARAMETER (Q = (6.0, 8.0) / (3.0, 4.0))
      PARAMETER (R = (2.0, 3.0) + 1.5, S = 4.0 - (1.0, -2.0))
      PARAMETER (T = (0.0, 1.0) ** 2, U = -(2.5, 0.5))
      PARAMETER (N = 2**10, M = 2**3**2, K = -2**2, L = 3 * 2**4 - 1)
      PARAMETER (X = 2.0**(-2), Y = 1.5**3)
      PARAMETER (D = 10.0D0**3)
      INTEGER V(2**2), W(-2**2:K+5)
      REAL*8 E1
      INTEGER*4 E2
      LOGICAL*4 E3
      IF (P .NE. (-5.0, 10.0)) STOP 1
      IF (Q .NE. (2.0, 0.0)) STOP 2
      IF (R .NE. (3.5, 3.0) .OR. S .NE. (3.0, 2.0)) STOP 3
      IF (T .NE. (-1.0, 0.0) .OR. U .NE. (-2.5, -0.5)) STOP 4
      IF (N .NE. 1024 .OR. M .NE. 512 .OR. K .NE. -4) STOP 5
      IF (L .NE. 47) STOP 6
      IF (X .NE. 0.25 .OR. Y .NE. 3.375 .OR. D .NE. 1000.0D0) STOP 7
      V(4) = 9
      W(-4) = 8
      W(1) = 7
      E1 = 0.75D0
      E2 = 12
      E3 = E2 .GT. 11
      IF (.NOT. E3) STOP 8
      WRITE (6, 100) N, M, K, L, V(4), W(-4), W(1), E1, E2
  100 FORMAT (1X, 'CPAR1', 7I6, F6.2, I4)
      WRITE (6, 101) P, Q, R, S, T, U
  101 FORMAT (1X, 'CPAR1', 12F6.2)
      STOP 0
      END
