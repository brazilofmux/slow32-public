C     Scalar-double-heavy kernel: a Mandelbrot-style iteration where
C     every hot value is a DOUBLE PRECISION scalar local, which is the
C     shape array-based LINPACK does not exercise.
      PROGRAM DSCAL2
      DOUBLE PRECISION X, Y, ZR, ZI, T, S
      INTEGER I, J, K, N
      N = 0
      DO 30 J = 1, 200
         Y = DBLE(J) / 100.0D0 - 1.0D0
         DO 20 I = 1, 200
            X = DBLE(I) / 100.0D0 - 1.5D0
            ZR = 0.0D0
            ZI = 0.0D0
            DO 10 K = 1, 60
               T = ZR*ZR - ZI*ZI + X
               ZI = 2.0D0*ZR*ZI + Y
               ZR = T
               S = ZR*ZR + ZI*ZI
               IF (S .GT. 4.0D0) GO TO 20
   10       CONTINUE
            N = N + 1
   20    CONTINUE
   30 CONTINUE
      IF (N .NE. 15756) STOP 1
      STOP 0
      END
