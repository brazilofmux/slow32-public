C     FORTRAN 90 ARRAY CONSTRUCTORS AS THE RIGHT SIDE OF AN ARRAY
C     ASSIGNMENT: WHOLE ARRAYS AND SECTIONS (STRIDED, A COLUMN, A ROW),
C     IMPLIED-DO VALUES NESTED, INTEGER VALUES FOR A REAL ARRAY, COMPLEX,
C     AND THE TARGET AMONG ITS OWN VALUES (EVALUATED BEFORE ANY STORE).
      PROGRAM F90AC1
      DOUBLE PRECISION X(8), ZERO, ONE
      PARAMETER (ZERO = 0.0D0, ONE = 1.0D0)
      INTEGER K(10), I, J, N
      REAL A(3,4)
      COMPLEX C(3)
      X = (/ -ONE, ZERO, -ONE, ZERO, ONE, ONE, ZERO, 2*ONE /)
      WRITE (*, 100) X
      K = (/ (I*I, I = 1, 10) /)
      WRITE (*, 200) K
      N = 3
      K(2:10:2) = (/ N, (I, -I, I = 1, 2) /)
      WRITE (*, 200) K
      K = (/ ((10*I+J, J = 1, I), I = 1, 4) /)
      WRITE (*, 200) K
      A = 0
      A(:, 2) = (/ 1, 2, 3 /)
      A(2, :) = (/ 5.5, 6.5, 7.5, 8.5 /)
      WRITE (*, 300) A
      X(1:3) = (/ X(3), X(2) + 1, X(1) * 4 /)
      WRITE (*, 100) X
      K(10:1:-1) = (/ (K(I), I = 1, 10) /)
      WRITE (*, 200) K
      C = (/ (1.0, 2.0), (3.0, -4.0), CMPLX(N, N) /)
      WRITE (*, 400) C
  100 FORMAT (1X, 8F6.1)
  200 FORMAT (1X, 10I4)
  300 FORMAT (1X, 12F5.1)
  400 FORMAT (1X, 6F5.1)
      END
