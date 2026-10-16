C     ** END DO and DO WHILE (MIL-STD-1753, 1978; not F77, but in every
C     compiler since).  An unlabelled DO ends at END DO; DO WHILE, with
C     and without a label; END DO as a labelled DO's terminal statement;
C     nesting, zero trips, a negative step, a jump out of the loop, a
C     block IF inside, and a small subroutine of them that can be
C     inlined into a loop.  From fort370.  Self-checking; success
C     reaches STOP 0.
      PROGRAM ENDDO1
      INTEGER I, J, K, N, W(4), G(2,3), T
      DO I = 1, 4
        W(I) = 2 * I + 1
      END DO
      IF (W(4) .NE. 9) STOP 1
      DO 15 J = 1, 3
        DO I = 1, 2
          G(I, J) = 100 * I + J
        END DO
   15 END DO
      IF (G(2, 3) .NE. 203) STOP 2
      T = 0
      DO 25, I = 1, 3
        DO J = 3, I, -1
          T = T + 10 * I + J
        END DO
   25 CONTINUE
      K = 0
      DO I = 4, 2
        K = K + 1
      END DO
      IF (K .NE. 0) STOP 3
      N = 2
      DO WHILE (N .LE. 50)
        N = N * N
      END DO
      IF (N .NE. 256) STOP 4
      K = 1
      DO 35 WHILE (K .LT. 20)
        K = K + 6
   35 CONTINUE
      IF (K .NE. 25) STOP 5
      J = 0
      DO I = 1, 12
        IF (MOD(I, 3) .EQ. 0) THEN
          J = J + I
        ELSE IF (I .GT. 9) THEN
          GO TO 45
        END IF
      END DO
   45 IF (I .NE. 10 .OR. J .NE. 18) STOP 6
      K = 0
      DO I = 2, 4
        CALL TRI(K, I)
      END DO
      IF (K .NE. 19) STOP 7
      WRITE (6, 100) W, G, T, N, K
  100 FORMAT (1X, 'ENDDO1', 4I3, 6I4, 3I5)
      STOP 0
      END
C
      SUBROUTINE TRI(K, N)
      INTEGER K, N, M
      DO M = 1, N
        K = K + M
      END DO
      END
