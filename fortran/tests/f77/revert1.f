C     ** Format reversion stays inside the format.  The reversion point
C     was the format's outer parenthesis, so reverting made the whole
C     format a group and its last ')' a group's end, not the format's;
C     the walk then went on past it.  A host format literal has a NUL
C     after it and hid this, but a format in a CHARACTER entity need
C     not: here the format is followed in COMMON by other text, and
C     five items through (1X,I3) printed two lines and lost the rest.
C     READ with (A) and three items is the same.  From fort370.
C     Self-checking; success reaches STOP 0.
      PROGRAM REVRT1
      CHARACTER*7 FMT
      CHARACTER*9 TAIL
      CHARACTER*12 REC(5)
      CHARACTER*4 A, B, C
      INTEGER I, N(5)
      COMMON /FB/ FMT, TAIL
      FMT = '(1X,I3)'
      TAIL = 'X,''zz'',/)'
      WRITE (REC, FMT) 11, 22, 33, 44, 55
      DO 10 I = 1, 5
         READ (REC(I), '(1X,I3)') N(I)
   10 CONTINUE
      IF (N(1) .NE. 11 .OR. N(3) .NE. 33 .OR. N(5) .NE. 55) STOP 1
      FMT = '(A)    '
      REC(1) = 'one'
      REC(2) = 'two'
      REC(3) = 'six'
      READ (REC, FMT) A, B, C
      IF (A .NE. 'one' .OR. B .NE. 'two' .OR. C .NE. 'six') STOP 2
      WRITE (6, 100) N, A, B, C
  100 FORMAT (1X, 'REVRT1', 5I4, 3(1X, A))
      STOP 0
      END
