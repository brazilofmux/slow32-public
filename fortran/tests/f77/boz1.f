C     Hexadecimal and octal DATA constants are the item's bits,
C     right-justified: zeros on the left of one too short, the leftmost
C     digits of one too long lost (MIL-STD-1753).  Pinned by boz1.expected
C     rather than gfortran, which refuses them for REAL items and for a
C     constant wider than its item.
      PROGRAM BOZ1
      REAL X, Y
      DOUBLE PRECISION D
      INTEGER I, J
      LOGICAL L
      DATA X /Z'3F800000'/, Y /Z'C0490FDB'/
      DATA D /Z'400921FB54442D18'/
      DATA I /Z'123456789'/, J /O'37777777777'/
      DATA L /Z'1'/
      IF (X .NE. 1.0) STOP 1
      IF (ABS(Y + 3.14159265) .GT. 1.0E-6) STOP 2
      IF (ABS(D - 3.14159265358979D0) .GT. 1.0D-13) STOP 3
      IF (I .NE. 591751049) STOP 4
      IF (J .NE. -1) STOP 5
      IF (.NOT. L) STOP 6
      WRITE (6, 100) X, I, J
  100 FORMAT (' BOZ ', F5.2, 2I11)
      STOP 0
      END
