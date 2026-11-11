C     A comma ends a numeric field short of its width; a field with no
C     digit before its exponent, or none at all, is zero.
      INTEGER I1, I2, I3
      REAL A, B, C
      OPEN (10, STATUS='SCRATCH')
      WRITE (10, '(A)') '1, 235'
      WRITE (10, '(A)') '1.5,2.25, 7'
      WRITE (10, '(A)') '           E+00'
      WRITE (10, '(A)') '+'
      REWIND 10
      READ (10, '(3I2)') I1, I2, I3
      PRINT *, I1, I2, I3
      READ (10, '(3F8.0)') A, B, C
      PRINT *, A, B, C
      A = 42.0
      READ (10, '(E15.8)') A
      B = 42.0
      READ (10, '(F10.4)') B
      PRINT *, A, B
      END
