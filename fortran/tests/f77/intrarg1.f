C     ** A bare intrinsic name in an argument list is the procedure only
C     when an INTRINSIC statement names it (F77 8.8, 15.10).  Without
C     one it is an implicitly typed variable, and a callee may store
C     through it.  The compiler had passed every such name as the
C     intrinsic, and refused IFIX, which cannot be passed at all.
C     Found by fort370.  Self-checking; success reaches STOP 0.
      PROGRAM INTARG
      INTRINSIC SQRT
      REAL R
      CALL PUTTWO(IFIX, MAX)
      IF (IFIX .NE. 41) STOP 1
      IF (MAX .NE. 58) STOP 2
      CALL APPLY(SQRT, 49.0, R)
      IF (ABS(R - 7.0) .GT. 1.0E-5) STOP 3
      WRITE (6,100) IFIX, MAX, R
  100 FORMAT (1X,'INTRARG',2I5,F7.2)
      STOP 0
      END
      SUBROUTINE PUTTWO(IA, IB)
      INTEGER IA, IB
      IA = 41
      IB = 58
      END
      SUBROUTINE APPLY(F, X, Y)
      REAL F, X, Y
      EXTERNAL F
      Y = F(X)
      END
