C     FORTRAN 90 ARRAY SECTIONS AS THE TARGET OF A SCALAR: (LO:HI),
C     EXPRESSION BOUNDS, A STRIDE, BOUNDS LEFT OUT ((:4), (7:)), 2-D
C     SECTIONS, A CONVERTED VALUE, COMPLEX*16, AND AN ASSUMED-SIZE DUMMY'S
C     SECTION WITH ITS UPPER BOUND GIVEN.
      PROGRAM F90SC1
      INTEGER IWORK(10), I, J, N
      DOUBLE PRECISION WORK(12), A(4,5), ZERO
      COMPLEX*16 Z(3)
      PARAMETER (ZERO = 0.0D0)
      DO 10 I = 1, 10
        IWORK(I) = I
   10 CONTINUE
      DO 20 I = 1, 12
        WORK(I) = DBLE(I)
   20 CONTINUE
      DO 30 J = 1, 5
      DO 30 I = 1, 4
        A(I,J) = 10 * I + J
   30 CONTINUE
      N = 3
      IWORK(1:4) = 0
      IWORK(N+4:N+6) = -1
      IWORK(2:10:4) = 99
      WORK(:4) = ZERO
      WORK(7:) = 1
      A(2:3, 4) = -2.5D0
      A(1, :) = 7
      A(3:4, 1:2) = 0.5
      Z(2:3) = (1.0D0, -1.0D0)
      CALL SUB(IWORK, 3)
      WRITE (*, 100) IWORK
      WRITE (*, 200) WORK
      WRITE (*, 200) A
      WRITE (*, 200) Z(1), Z(2), Z(3)
  100 FORMAT (1X, 10I4)
  200 FORMAT (1X, 6F8.2)
      END
      SUBROUTINE SUB(K, M)
      INTEGER K(*), M
      K(M+1:M+2) = 42
      END
