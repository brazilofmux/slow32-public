C     NAMELIST WRITE then READ back: every kind of item, an array, a
C     string with its quote, and the values after the round trip.
      PROGRAM NMLRT
      INTEGER K, L(3)
      REAL R, A(2)
      DOUBLE PRECISION G
      COMPLEX Z
      LOGICAL Q
      CHARACTER*7 H
      NAMELIST /DESK/ K, L, R, A, G, Z, Q, H
      K = -4096
      L(1) = 7
      L(2) = 0
      L(3) = 123456
      R = 0.375
      A(1) = -1.5E6
      A(2) = 2.0E-4
      G = 98765.4321D0
      Z = (-0.5, 3.25)
      Q = .TRUE.
      H = 'TOM''S'
      WRITE (12, DESK)
      REWIND 12
      K = 0
      L(1) = 0
      L(3) = 0
      R = 0.0
      A(1) = 0.0
      A(2) = 0.0
      G = 0.0D0
      Z = (0.0, 0.0)
      Q = .FALSE.
      H = ' '
      READ (12, DESK)
      WRITE (6, 101) K, L, R, A, G, Z, Q, H
  101 FORMAT (1X, I6, 3I7, F7.3, 2E11.3, F12.4, 2F6.2, L2, 1X, A)
      END
