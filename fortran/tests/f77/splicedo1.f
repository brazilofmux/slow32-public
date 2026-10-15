C     ** A dummy of a subprogram inlined in place of its CALL is not
C     loop-invariant in the caller.  A load of a dummy is assumed
C     invariant, because nothing else may change it while its own unit
C     runs; inlined, the callee runs inside the caller, whose DO is
C     changing the actual.  RAMP's V = 3*K + 1 was hoisted out of the
C     loop and every V was the first trip's.  TALLY keeps its CALL (a
C     unit with EQUIVALENCE is never inlined), and passing it K keeps
C     K in storage, where the hoisted load read it once.  From
C     fort370.  Self-checking; success reaches STOP 0.
      PROGRAM SPLDO1
      INTEGER K, ISUM
      DOUBLE PRECISION V
      COMMON /ACC/ ISUM
      ISUM = 0
      DO 10 K = 1, 4
         CALL RAMP(K, V)
         CALL TALLY(K, V)
   10 CONTINUE
      IF (ISUM .NE. 34) STOP 1
      WRITE (6, 100) ISUM
  100 FORMAT (1X, 'SPLDO1', I5)
      STOP 0
      END
C
      SUBROUTINE RAMP(K, V)
      INTEGER K
      DOUBLE PRECISION V
      V = DBLE(FLOAT(3 * K + 1))
      RETURN
      END
C
      SUBROUTINE TALLY(K, X)
      INTEGER K, IW(2), ISUM
      DOUBLE PRECISION X, W
      COMMON /ACC/ ISUM
      EQUIVALENCE (W, IW(1))
      W = X
      IF (K .GT. 0) ISUM = ISUM + IDINT(W)
      RETURN
      END
