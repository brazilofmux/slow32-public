C     ** An intrinsic function's name is the intrinsic only where it is
C     referenced as a function.  Not followed by ( it is a variable of
C     the unit (LEN here), and an array of that name is the array (MAX).
C     The compiler had taken every LEN as the intrinsic and stopped with
C     "intrinsic needs arguments"; MIN is still the intrinsic.  Found
C     by fort370.  Self-checking; success reaches STOP 0.
      INTEGER LEN, MAX(3), K, I
      LEN = 30
      DO 10 I = 1, 3
        MAX(I) = I * LEN
   10 CONTINUE
      K = 0
      DO 20 I = 1, LEN
        K = K + 1
   20 CONTINUE
      IF (K .NE. 30) STOP 1
      IF (MAX(3) .NE. 90) STOP 2
      IF (MIN(LEN, MAX(2)) .NE. 30) STOP 3
      WRITE (6,100) LEN, K, MAX(3), MIN(LEN, MAX(2))
  100 FORMAT (1X,'INTRVAR',4I5)
      STOP 0
      END
