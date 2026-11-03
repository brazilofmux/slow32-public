C     I/O inside I/O stops the run (F77; fort370 issue 39, from MV410).
C     I2A does an internal WRITE.  Referenced in an assignment it is
C     fine; a READ loop left by END= must leave the I/O state free; then
C     I2A in a PRINT list is an I/O statement inside another's, and the
C     run stops -- where it used to lose the outer record and end 0.
      PROGRAM IORECR
      CHARACTER*8 I2A, S
      INTEGER K, T
      S = I2A(42)
      PRINT *, 'ASSIGNED ', S
      T = 0
   10 READ (5, *, END=20) K
      T = T + K
      GO TO 10
   20 PRINT *, 'TOTAL ', T
      PRINT *, 'N = '//I2A(1234)//'.'
      PRINT *, 'NOT REACHED'
      END
      CHARACTER*8 FUNCTION I2A(N)
      INTEGER N
      WRITE (I2A, '(I8)') N
      END
