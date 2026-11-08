C     COMPLEX*16 ARITHMETIC: + - * WITH MIXED TYPES (COMPLEX WITH DOUBLE
C     PRECISION IS COMPLEX*16), UNARY MINUS, DIVISION (OPERANDS WHOSE
C     QUOTIENTS ARE EXACT: THE ALGORITHMS DIFFER IN THE LAST BIT
C     ELSEWHERE), ** BY AN INTEGER IN LIBGFORTRAN'S ORDER, .EQ. AND .NE.,
C     ABS AND CDABS.
      PROGRAM DCPLX3
      COMPLEX*16 A, B, C, Z(3)
      COMPLEX S
      DOUBLE PRECISION D
      INTEGER I, N
      LOGICAL L1, L2, L3
      A = (1.5D0, -2.25D0)
      B = (0.75D0, 0.5D0)
      S = (2.0, 1.0)
      D = 3.0D0
      C = A + B
      CALL SHOW(C)
      C = A - B
      CALL SHOW(C)
      C = A * B
      CALL SHOW(C)
      C = A * D + S
      CALL SHOW(C)
      C = 2 * A - D
      CALL SHOW(C)
      C = -A
      CALL SHOW(C)
      C = (4.0D0, 2.0D0) / (1.0D0, 1.0D0)
      CALL SHOW(C)
      C = (6.0D0, 8.0D0) / 2.0D0
      CALL SHOW(C)
      C = (3.0D0, 4.0D0) / (0.0D0, 2.0D0)
      CALL SHOW(C)
      C = A ** 2
      CALL SHOW(C)
      C = A ** 5
      CALL SHOW(C)
      C = (1.0D0, 1.0D0) ** (-2)
      CALL SHOW(C)
      N = 0
      C = A ** N
      CALL SHOW(C)
      L1 = A .EQ. A
      L2 = A .NE. B
      L3 = A .EQ. (1.5D0, -2.25D0)
      WRITE (*, 200) L1, L2, L3, A .EQ. B
      WRITE (*, 100) ABS((3.0D0, 4.0D0)), CDABS(A)
      DO 10 I = 1, 3
        Z(I) = DCMPLX(DBLE(I), -DBLE(I)) * A
   10 CONTINUE
      C = Z(1) + Z(2) * Z(3) - A
      CALL SHOW(C)
      S = A * S
      WRITE (*, 100) DBLE(REAL(S)), DBLE(AIMAG(S))
  100 FORMAT (2F24.15)
  200 FORMAT (4L3)
      END
      SUBROUTINE SHOW(C)
      COMPLEX*16 C
      WRITE (*, 100) DREAL(C), DIMAG(C)
  100 FORMAT (2F24.15)
      END
