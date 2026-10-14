C     ** Values that trade registers on a loop's back edge: the phi
C     copies form a cycle.  fort370's S/370 copy sequencer broke such a
C     cycle wrongly; ours parks through r2 and passes, and this holds
C     it there (FPR-sized and integer swaps).  Self-checking; success
C     reaches STOP 0.
      REAL X, Y, T, U, V, W
      INTEGER I, J, K, L
      X = 1.
      Y = 2.
      U = 3.
      V = 4.
      I = 5
      J = 6
      DO 10 N = 1, 5
         T = X
         X = Y
         Y = T
         W = U
         U = V
         V = W
         K = I
         I = J
         J = K
   10 CONTINUE
      WRITE (6, 100) X, Y, U, V, I, J
  100 FORMAT (1X, '>PHISWAP', 4F5.1, 2I3)
      IF (X .NE. 2. .OR. Y .NE. 1.) STOP 1
      IF (U .NE. 4. .OR. V .NE. 3.) STOP 2
      IF (I .NE. 6 .OR. J .NE. 5) STOP 3
      STOP 0
      END
