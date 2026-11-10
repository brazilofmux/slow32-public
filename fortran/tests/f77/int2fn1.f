C     An INTEGER*2 FUNCTION's result in an output list is a halfword: two
C     bytes of an unformatted record, seven columns list-directed.  It
C     was one only when the function was spliced into its caller (the
C     result variable's load); a real call wrote a fullword, and the
C     record read back as 105 and 0.  G has a SAVE and is never spliced;
C     F is, unless the inliner is off.
      PROGRAM I2FN
      INTEGER*2 K, F, G, A, B
      LOGICAL*1 T, LA, LB
      EXTERNAL G
      CHARACTER*40 LINE
      K = 100
      OPEN (9, FILE='int2fn1.dat', FORM='UNFORMATTED', STATUS='UNKNOWN')
      WRITE (9) F(K), G(K)
      WRITE (9) K
      WRITE (9) T(1), T(-1), F(K)
      REWIND 9
      READ (9) A, B
      PRINT *, A, B
      IF (A .NE. 105 .OR. B .NE. 101) STOP 1
      READ (9) A
      PRINT *, A
      READ (9) LA, LB, A
      PRINT *, LA, LB, A
      IF (A .NE. 105) STOP 2
      CLOSE (9, STATUS='DELETE')
      WRITE (LINE, *) F(K), G(K), K
      PRINT *, LINE
      PRINT *, F(K), G(K), F(K) + 1
      CALL SUB(G, K)
      STOP 0
      END
      INTEGER*2 FUNCTION F(K)
      INTEGER*2 K
      F = K + 5
      END
      INTEGER*2 FUNCTION G(K)
      INTEGER*2 K
      INTEGER N
      SAVE N
      DATA N /0/
      N = N + 1
      IF (N .GT. 100) STOP 9
      G = K + N
      END
      LOGICAL*1 FUNCTION T(I)
      INTEGER I
      INTEGER N
      SAVE N
      DATA N /0/
      N = N + 1
      T = I .GT. 0
      END
      SUBROUTINE SUB(FN, K)
      INTEGER*2 FN, K
      EXTERNAL FN
      PRINT *, FN(K), 5
      END
