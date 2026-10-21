C     INTEGER*2 is a halfword and LOGICAL*1 a byte: their storage is that
C     size (arrays, EQUIVALENCE with fullwords, DATA, COMMON, records),
C     a store keeps the low 16 bits, and a function's result, a dummy
C     and a DO variable follow.  (Their arithmetic is in fullwords, as in
C     FORTRAN H, where other compilers work at 16 bits: int2h1.)
      PROGRAM INT2W
      INTEGER*2 H(4), HS, HD
      INTEGER W(2)
      LOGICAL*1 FLAGS(8)
      INTEGER WF(2)
      INTEGER*2 TAB(5)
      INTEGER*2 PAIR(2)
      INTEGER J, K, ISUM
      INTEGER*2 TWICE
      COMMON /SHARED/ PAIR, J
      EQUIVALENCE (H, W)
      EQUIVALENCE (FLAGS, WF)
      DATA TAB /-1, 2, -3, 32767, -32768/
      H(1) = 1
      H(2) = 2
      H(3) = -1
      H(4) = 300
      WRITE (6, 101) W
      DO 5 I = 1, 8
    5 FLAGS(I) = MOD(I, 3) .EQ. 0
      WRITE (6, 102) FLAGS
      WF(1) = 0
      WRITE (6, 102) FLAGS
      HS = 30000
      J = HS
      WRITE (6, 103) HS, J
      K = 60000
      HS = K
      WRITE (6, 103) HS, K
      WRITE (6, 104) TAB
      ISUM = 0
      DO 10 HD = 1, 5
   10 ISUM = ISUM + TAB(HD) * HD
      WRITE (6, 103) HD, ISUM
      PAIR(1) = 11
      PAIR(2) = -12
      J = 13
      CALL SHOW
      WRITE (6, 103) TWICE(TAB(4)), TWICE(PAIR(2))
      CALL BUMP(PAIR(1))
      WRITE (6, 103) PAIR(1), PAIR(2)
      WRITE (14) H, TAB(5), FLAGS(3)
      REWIND 14
      H(1) = 0
      H(4) = 0
      TAB(5) = 0
      READ (14) H, TAB(5), FLAGS(3)
      WRITE (6, 105) H, TAB(5), FLAGS(3)
  101 FORMAT (1X, 2I12)
  102 FORMAT (1X, 8L2)
  103 FORMAT (1X, 2I8)
  104 FORMAT (1X, 5I8)
  105 FORMAT (1X, 5I7, L2)
      END
      SUBROUTINE SHOW
      INTEGER*2 PAIR(2)
      INTEGER J
      COMMON /SHARED/ PAIR, J
      WRITE (6, 101) PAIR, J
  101 FORMAT (1X, 3I6)
      END
      INTEGER*2 FUNCTION TWICE(N)
      INTEGER*2 N
      TWICE = N + N
      END
      SUBROUTINE BUMP(N)
      INTEGER*2 N
      N = N + 1000
      END
