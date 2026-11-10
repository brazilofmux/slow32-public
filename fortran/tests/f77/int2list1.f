C     List-directed INTEGER*2: seven columns, where a fullword has
C     twelve -- gfortran's layout, like the rest of list-directed output.
C     An expression of halfwords is a fullword, and so is K + 1.
      PROGRAM I2LIST
      INTEGER*2 K, H(3)
      LOGICAL*1 B
      INTEGER*2 F
      CHARACTER*40 LINE
      K = -32768
      H(1) = 1
      H(2) = -20
      H(3) = 32767
      B = .TRUE.
      PRINT *, K, 7, H, B
      PRINT *, H(2), K + 1, F(K), (H(I), I = 1, 3)
      WRITE (*, *) H
      WRITE (LINE, *) H(3), K, 5
      PRINT *, LINE
      STOP 0
      END
      INTEGER*2 FUNCTION F(K)
      INTEGER*2 K
      F = K + 5
      END
