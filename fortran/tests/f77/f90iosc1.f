C     FORTRAN 90 ARRAY SECTIONS IN I/O LISTS: WRITE OF 1-D, 2-D AND STRIDED
C     (BACKWARD TOO) SECTIONS, AND A LIST-DIRECTED READ INTO A SECTION.
      PROGRAM F90IO1
      DOUBLE PRECISION Z(6,3)
      INTEGER K(8), I, J
      CHARACTER*40 LINE
      DO 10 J = 1, 3
      DO 10 I = 1, 6
        Z(I,J) = 10 * I + J
   10 CONTINUE
      DO 20 I = 1, 8
        K(I) = I * I
   20 CONTINUE
      WRITE (*, 100) Z(1:6, 3)
      WRITE (*, 100) Z(2:3, 1:3), Z(6, 1)
      WRITE (*, 200) K(2:8:3), K(8:1:-2)
      LINE = '7 8 9'
      READ (LINE, *) K(3:7:2)
      WRITE (*, 200) K
  100 FORMAT (1X, 7F7.1)
  200 FORMAT (1X, 8I5)
      END
