C     ** A main program's arrays are static, as FORTRAN H kept them.
C     They were stack locals, and an array past the linker's 64 KB
C     default stack -- INTEGER A(30000) in a pi spigot posted to
C     H390-MVS -- faulted on its first store.  Here 160 KB of INTEGER,
C     80 KB of DOUBLE PRECISION and 70 KB of CHARACTER, filled, passed
C     to a subprogram and summed.  Self-checking; success reaches
C     STOP 0.
      PROGRAM BIGARR
      INTEGER A(40000), I, ISUM
      DOUBLE PRECISION D(10000), DSUM
      CHARACTER*7 C(10000)
      DO 10 I = 1, 40000
         A(I) = MOD(I, 7)
10    CONTINUE
      DO 20 I = 1, 10000
         D(I) = 0.5D0 * I
         C(I) = 'SPIGOT '
20    CONTINUE
      CALL TWICE(A, 40000)
      ISUM = 0
      DO 30 I = 1, 40000
         ISUM = ISUM + A(I)
30    CONTINUE
      DSUM = 0.0D0
      DO 40 I = 1, 10000
         DSUM = DSUM + D(I)
40    CONTINUE
      IF (ISUM .NE. 239994) STOP 1
      IF (DSUM .NE. 25002500.0D0) STOP 2
      IF (C(10000) .NE. 'SPIGOT ') STOP 3
      WRITE (6, 100) ISUM, DSUM, C(1)
100   FORMAT (1X, I8, F12.1, 1X, A7)
      STOP 0
      END
      SUBROUTINE TWICE(K, N)
      INTEGER K(N), N, J
      DO 10 J = 1, N
         K(J) = 2 * K(J)
10    CONTINUE
      END
