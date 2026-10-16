C     ** COMPLEX division away from 1.  It was done inline in REAL,
C     where the divisor's BR**2 + BI**2 overflows past about 1.8E19
C     and underflows below about 1E-19, so (1E20,1E20) / (2E20,0) was
C     NaN.  It is the runtime's now, in DOUBLE PRECISION with the
C     divisor scaled.  Each quotient here is exact; a sweep of ordinary
C     values checks the error stays within a few units.  From fort370.
C     Self-checking; success reaches STOP 0.
      PROGRAM CDIV1
      COMPLEX Z, W, Q
      REAL AR, AI, BR, BI, E
      DOUBLE PRECISION DR, DI, DEN
      INTEGER I, IS
      Z = (1.0E20, 1.0E20)
      W = (2.0E20, 0.0)
      Q = Z / W
      IF (Q .NE. (0.5, 0.5)) STOP 1
      Z = (3.0E-25, 4.0E-25)
      W = (1.0E-25, 1.0E-25)
      Q = Z / W
      IF (Q .NE. (3.5, 0.5)) STOP 2
      Z = (-6.0E30, 8.0E30)
      W = (3.0E30, 4.0E30)
      Q = Z / W
      IF (Q .NE. (0.56, 1.92)) THEN
         IF (ABS(REAL(Q) - 0.56) .GT. 1.0E-6) STOP 3
         IF (ABS(AIMAG(Q) - 1.92) .GT. 1.0E-6) STOP 4
      END IF
      W = (3.0, 4.0)
      Q = (25.0, 0.0) / W
      IF (Q .NE. (3.0, -4.0)) STOP 5
      IS = 4321
      DO 10 I = 1, 500
         IS = MOD(IS * 1103 + 7, 32749)
         AR = REAL(IS) / 3001.0 - 5.0
         IS = MOD(IS * 1103 + 7, 32749)
         AI = REAL(IS) / 2999.0 - 5.0
         IS = MOD(IS * 1103 + 7, 32749)
         BR = REAL(IS) / 3011.0 - 5.0
         IS = MOD(IS * 1103 + 7, 32749)
         BI = REAL(IS) / 2989.0 - 5.0
         Q = CMPLX(AR, AI) / CMPLX(BR, BI)
         DEN = DBLE(BR)**2 + DBLE(BI)**2
         DR = (DBLE(AR)*BR + DBLE(AI)*BI) / DEN
         DI = (DBLE(AI)*BR - DBLE(AR)*BI) / DEN
         E = SQRT((REAL(Q) - DR)**2 + (AIMAG(Q) - DI)**2)
     1       / (SQRT(DR*DR + DI*DI) * 5.96E-8)
         IF (E .GT. 4.0) STOP 6
   10 CONTINUE
      WRITE (6, 100) REAL(Q), AIMAG(Q)
  100 FORMAT (1X, 'CDIV1', 2F10.4)
      STOP 0
      END
