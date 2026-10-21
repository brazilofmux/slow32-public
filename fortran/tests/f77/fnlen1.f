C     A function's length after its name: INTEGER FUNCTION F*2(I), as
C     FORTRAN H writes it, gives a halfword result; CHARACTER FUNCTION
C     C*3(I), standard F77, a 3-character one.
      PROGRAM FNLEN
      INTEGER*2 HALVE
      CHARACTER*3 TAG
      INTEGER M
      M = 70000
      WRITE (6, 101) HALVE(M), TAG(2)
  101 FORMAT (1X, I8, 1X, A)
      END
      INTEGER FUNCTION HALVE*2(N)
      INTEGER N
      HALVE = N
      END
      CHARACTER FUNCTION TAG*3(K)
      INTEGER K
      TAG = 'AB' // CHAR(ICHAR('0') + K)
      END
