C     ** A subprogram inlined in place of its call keeps its own typing
C     rules.  The inliner reads the callee's statements in the
C     caller's context and left the caller's IMPLICIT in force: under
C     IMPLICIT INTEGER (A-Z) the callee's HALF = 0.5 stored 0, with no
C     diagnostic, and under IMPLICIT NONE the callee's own undeclared
C     names were refused, the error naming the callee's header.  A
C     unit with an IMPLICIT of its own is never inlined, so the callee
C     always has the default rule.  From fort370, which found it on a
C     user's program.  Self-checking; success reaches STOP 0.
      PROGRAM IMPLN1
      IMPLICIT INTEGER (A-Z)
      M = 10
      CALL SCALE (M)
      IF (M .NE. 15) STOP 1
      CALL STRICT (K)
      IF (K .NE. 42) STOP 2
      CALL LOOSE (L)
      IF (L .NE. 9) STOP 3
      WRITE (6, 100) M, K, L
  100 FORMAT (1X, 'IMPLN1', 3I6)
      STOP 0
      END
C
      SUBROUTINE SCALE (M)
      HALF = 0.5
      M = M + INT(HALF * M)
      END
C
      SUBROUTINE STRICT (K)
      IMPLICIT NONE
      INTEGER K, NEXT6
      K = NEXT6(36)
      END
C
      INTEGER FUNCTION NEXT6 (J)
      INTEGER J
      NEXT6 = J + 6
      RETURN
      END
C
      SUBROUTINE LOOSE (L)
      TOTAL = 4.5
      L = INT(TOTAL + TOTAL)
      END
