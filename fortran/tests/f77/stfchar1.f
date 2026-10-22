C     A CHARACTER actual of a statement function reaches its dummy as
C     characters, cut to the dummy's length (F77 15.4.2): a substring,
C     an array element, a constant and a whole variable.  The dummy
C     used to get the low byte of the actual's address instead.
      PROGRAM STFCH1
      CHARACTER*10 T
      CHARACTER K, E*4, A(2)*6
      LOGICAL ISDIG
      INTEGER IP, IB
      ISDIG(K) = K .GE. '0' .AND. K .LE. '9'
      IP(E) = INDEX(E, 'P')
      IB(E) = INDEX(E, ' ')
      T = 'A1 9;P4B'
      A(1) = 'XYPZWV'
      A(2) = 'HU'
      N = 0
      DO 10 I = 1, 10
         IF (ISDIG(T(I:I))) N = N + 1
   10 CONTINUE
      J = 4
      WRITE (6, 100) N, ISDIG(T(1:1)), ISDIG(T(J:J)), ISDIG('7')
      WRITE (6, 101) IP(T(5:10)), IP(A(1)), IP(T), IB(A(2)), IB('CD E')
      IF (N .NE. 3) STOP 1
      IF (ISDIG(T(1:1)) .OR. .NOT. ISDIG(T(J:J))) STOP 2
      IF (.NOT. ISDIG('7')) STOP 3
      IF (IP(T(5:10)) .NE. 2 .OR. IP(A(1)) .NE. 3) STOP 4
      IF (IP(T) .NE. 0 .OR. IB(A(2)) .NE. 3 .OR. IB('CD E') .NE. 3) STOP 5
  100 FORMAT (' DIGITS ', I3, 3L2)
  101 FORMAT (' FOUND ', 5I3)
      STOP 0
      END
