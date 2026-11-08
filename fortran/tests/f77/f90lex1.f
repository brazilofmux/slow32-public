C     FORTRAN 90 IN FIXED FORM, LEXICALLY: == /= < <= > >= (IN IF AND IN A
C     LOGICAL ASSIGNMENT; /= BESIDE A SLASH), "..." CONSTANTS WITH '' AND
C     "" INSIDE AND A ! INSIDE, A TRAILING ! COMMENT, AND AN INDENTED ! LINE,
C     WHICH HAD BEEN AN EMPTY STATEMENT THAT LOST THE UNIT ITS LABELS.
      PROGRAM F90LX1
      INTEGER I, J, K(3)
      LOGICAL L1, L2
      CHARACTER*12 S, T
      DATA K /1, 2, 3/
      I = 3
      J = 4
      IF (I == 3) WRITE (*, 100) 'EQ'
      IF (I /= J) WRITE (*, 100) 'NE'
      IF (I < J .AND. J > I) WRITE (*, 100) 'LT GT'
      IF (I <= 3 .AND. J >= 4) WRITE (*, 100) 'LE GE'
      L1 = I == J
      L2 = K(2)/=K(3)
      WRITE (*, 200) L1, L2
         ! an indented comment line
      S = "it's ""ok"""
      T = 'A "Q" B'
      WRITE (*, 100) S
      WRITE (*, 100) T
      WRITE (*, 100) "bang ! inside"   ! a trailing comment
      DO 10 I = 1, 2
        IF (K(I)/=0) J = J + K(I)
   10 CONTINUE
      WRITE (*, 300) J
  100 FORMAT (1X, A)
  200 FORMAT (1X, 2L2)
  300 FORMAT (1X, I4)
      END
