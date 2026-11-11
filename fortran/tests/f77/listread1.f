      COMPLEX A(4), B
      DOUBLE PRECISION D(3)
      INTEGER K(3)
      CHARACTER*40 T
      OPEN (19, STATUS='SCRATCH')
      WRITE (19, *) '4*(1.0,2.0)  (3.0 , 4.5)'
      WRITE (19, *) '2*1.5+3 7.25D0 3*5'
      WRITE (19, '(A)') ' 1.5+2  -3.25E1    .    -42  +7'
      WRITE (19, '(A)') '   Inf  -Infinity   NaN'
      REWIND 19
      READ (19, *) A, B
      PRINT *, A, B
      READ (19, *) D, K
      PRINT *, D, K
      READ (19, '(F6.0,F9.0,F5.0,2X,I6,I4)') D, K(1), K(2)
      PRINT *, D, K(1), K(2)
      READ (19, '(F6.0,F11.0,F6.0)') D
      PRINT *, D
      T = '  12  3.5  x'
      READ (T, '(I4,F5.0)') K(1), D(1)
      PRINT *, K(1), D(1)
      END
