C     An implied DO in an I/O list with a REAL or DOUBLE PRECISION
C     variable: the variable steps and the trips are counted in its own
C     type, as a DO statement's are.  They were stored and counted as
C     integers, and the list printed nothing or garbage.
      PROGRAM DOREAL
      REAL X, R(4)
      DOUBLE PRECISION D
      INTEGER I, N
      WRITE (*, *) (X, X = 1.0, 2.0, 0.5)
      WRITE (*, *) (D * 2, D = 1.0D0, 2.5D0, 0.5D0)
      WRITE (*, *) (I, I = 1.5, 3.9)
      WRITE (*, *) X, D, I
      WRITE (*, *) (X, X = 2.0, 1.0, -0.25), X
      WRITE (*, *) (D, D = 3, 1, -1), D
      WRITE (*, *) 'none', (X, X = 2.0, 1.0), X
      WRITE (*, *) ((X + I, X = 0.25, 0.5, 0.25), I = 1, 2)
      WRITE (*, 20) (X * X, X = 0.5, 2.0, 0.5)
20    FORMAT (4F8.3)
      N = 0
      DO 10 X = 1.0, 2.0, 0.25
        N = N + 1
        R(MIN(N, 4)) = X
10    CONTINUE
      WRITE (*, *) N, X, R
      READ (*, *) (R(INT(X)), X = 1.0, 3.0)
      WRITE (*, *) R, X
      IF (N .NE. 5) STOP 1
      IF (X .NE. 4.0) STOP 2
      STOP 0
      END
