C     FORTRAN 90 AUTOMATIC ARRAYS: LOCALS WHOSE BOUNDS COME FROM DUMMIES,
C     AS LAPACK'S DTRSYL3 HAS W(MAX(M,N)) -- 1-D AND 2-D, WITH A LOWER
C     BOUND, PASSED ON TO ANOTHER ROUTINE, IN A SMALL UNIT CALLED IN A
C     LOOP (ONE THAT WOULD BE INLINED), BESIDE AN ARRAY OF ITS OWN.
      PROGRAM AUTO1
      INTEGER I
      DOUBLE PRECISION T
      T = 0
      DO 10 I = 1, 30
        CALL SUMSQ(I, 40 - I, T)
   10 CONTINUE
      WRITE (*, '(F20.4)') T
      CALL TWOD(3, 5)
      END

      SUBROUTINE SUMSQ(M, N, T)
      INTEGER M, N, I
      DOUBLE PRECISION T, W(MAX(M, N)), Z(2)
      Z(1) = 1
      Z(2) = 2
      DO 10 I = 1, MAX(M, N)
        W(I) = I * Z(2) - Z(1)
   10 CONTINUE
      CALL ADDSQ(MAX(M, N), W, T)
      END

      SUBROUTINE ADDSQ(N, W, T)
      INTEGER N, I
      DOUBLE PRECISION W(N), T
      DO 10 I = 1, N
        T = T + W(I)**2
   10 CONTINUE
      END

      SUBROUTINE TWOD(M, N)
      INTEGER M, N, I, J
      REAL A(M, N), B(-1:M)
      DO 20 J = 1, N
        DO 10 I = 1, M
          A(I, J) = 10 * I + J
   10   CONTINUE
   20 CONTINUE
      DO 30 I = -1, M
        B(I) = I * 0.5
   30 CONTINUE
      WRITE (*, '(5F6.1)') ((A(I, J), J = 1, N), I = 1, M)
      WRITE (*, '(5F6.1)') B
      END
