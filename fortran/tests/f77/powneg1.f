C     X**(-N) whose X**N would overflow is (1/X)**N, so the result is
C     the tiny number it should be, not 0 (from fort370, after FORTRAN H's
C     library).  Pinned to powneg1.expected rather than to gfortran: its
C     run time computes 1/X**N, overflows, and prints 0 for all four --
C     though it folds the same expressions with constant exponents to
C     these values.  Exponents in variables, so the run time is used.
      PROGRAM POWNEG
      REAL X, R1, R2
      DOUBLE PRECISION Y, D1, D2
      KA = -70
      KB = -140
      KC = -520
      KD = -350
      X = 4E0
      R1 = X**KA
      X = 2E0
      R2 = X**KB
      Y = 4D0
      D1 = Y**KC
      Y = 8D0
      D2 = Y**KD
      WRITE (6, 10) R1, R2
      WRITE (6, 10) D1, D2
   10 FORMAT (1X, 2E14.5)
      IF (R1 .LE. 0.0 .OR. R2 .LE. 0.0) STOP 1
      IF (D1 .LE. 0D0 .OR. D2 .LE. 0D0) STOP 2
C     and nothing that fits changes: these never come near the limit
      Y = 3D0
      IF (Y**(KB/10) .NE. 1D0/3D0**14) STOP 3
      STOP 0
      END
