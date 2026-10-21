C     INTEGER*2 arithmetic is in fullwords, as FORTRAN H does it: the
C     operands are widened, the sum and product are full INTEGERs, and
C     only a store to an INTEGER*2 keeps 16 bits.  Compilers that work
C     at 16 bits print -5536 and 9216 here, so the expected output is H's.
      PROGRAM INT2H
      INTEGER*2 A, B, C
      INTEGER J, K
      A = 30000
      B = 200
      J = A + A
      K = A * B
      C = A + A
      WRITE (6, 101) J, K, C
  101 FORMAT (1X, 3I10)
      END
