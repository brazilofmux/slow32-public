C     A subprogram's COMMON, SAVEd, DATA'd and EQUIVALENCEd arrays are
C     static, and take none of its frame: each of these is larger than
C     the whole 64 KB stack.  (Every array had its frame bytes from the
C     moment it was declared, static or not.)
      PROGRAM CFRAME
      INTEGER TOTAL
      COMMON /TOT/ TOTAL
      TOTAL = 0
      CALL S1(3)
      CALL S2(4)
      CALL S2(5)
      CALL S3(6)
      CALL S4(7)
      PRINT *, TOTAL
      END
      SUBROUTINE S1(K)
      INTEGER TOTAL, A, B
      COMMON /TOT/ TOTAL
      COMMON /BIG/ A(30000), B(3, 20000)
      A(K) = K
      A(30000) = 1
      B(3, 20000) = 2
      TOTAL = TOTAL + A(K) + A(30000) + B(3, 20000)
      PRINT *, 'S1', A(K), TOTAL
      END
      SUBROUTINE S2(K)
      INTEGER TOTAL
      COMMON /TOT/ TOTAL
      DOUBLE PRECISION KEPT(20000)
      INTEGER CALLS
      SAVE KEPT, CALLS
      DATA CALLS /0/
      CALLS = CALLS + 1
      IF (CALLS .EQ. 1) KEPT(20000) = 0.5D0
      KEPT(20000) = KEPT(20000) + K
      TOTAL = TOTAL + CALLS
      PRINT *, 'S2', CALLS, KEPT(20000)
      END
      SUBROUTINE S3(K)
      INTEGER TOTAL
      COMMON /TOT/ TOTAL
      INTEGER TABLE(25000)
      DATA TABLE(6), TABLE(25000) /9, 9/
      TOTAL = TOTAL + TABLE(K) + TABLE(25000)
      PRINT *, 'S3', TABLE(K) + TABLE(25000), TABLE(1)
      END
      SUBROUTINE S4(K)
      INTEGER TOTAL
      COMMON /TOT/ TOTAL
      REAL X(20000), Y(20000)
      EQUIVALENCE (X(1), Y(2))
      INTEGER LOCAL(8)
      LOCAL(8) = K
      Y(2) = 1.5
      Y(20000) = 2.5
      TOTAL = TOTAL + LOCAL(8)
      PRINT *, 'S4', X(1), X(19999), LOCAL(8)
      END
