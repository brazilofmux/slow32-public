C     ** Type lengths.  REAL*8 FUNCTION was not taken for a header (the
C     unit became a second main program), and a name starting with H
C     after a length -- REAL*8 HALF, CHARACTER*8 HUE -- was read as a
C     Hollerith constant once the blanks were gone (*8H...).  A length
C     the compiler cannot honour is refused now, not taken as the
C     default size; tests/run-tests.sh checks those.  From fort370,
C     which found the header.  Self-checking; success reaches STOP 0.
      PROGRAM REAL8F
      REAL*8 THIRD, HALF, H
      REAL*4 R
      INTEGER*4 N
      LOGICAL*4 OK
      COMPLEX*8 Z
      CHARACTER*8 HUE
      HALF = 0.5D0
      H = THIRD(HALF)
      IF (ABS(H - 1.0D0 / 6.0D0) .GT. 1.0D-15) STOP 1
      R = 2.5
      N = 7
      OK = N .GT. 6
      IF (.NOT. OK) STOP 2
      Z = (1.0, -1.0)
      HUE = 'teal'
      IF (HUE .NE. 'teal    ') STOP 3
      WRITE (6, 100) HUE, H, R, N, REAL(Z)
  100 FORMAT (1X, 'REAL8F ', A, F12.8, F6.2, I4, F6.2)
      STOP 0
      END
C
      REAL*8 FUNCTION THIRD(Y)
      REAL*8 Y
      THIRD = Y / 3.0D0
      END
