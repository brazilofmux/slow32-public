C     The COMPLEX specific intrinsics passed as actual arguments (F77
C     15.9.3, table 5): CSQRT, CEXP, CLOG, CSIN, CCOS, CONJG, and with
C     them CABS and AIMAG.  Each, called through the dummy, must give
C     what the intrinsic gives called by name.
      PROGRAM CXARG1
      INTRINSIC CSQRT, CEXP, CLOG, CSIN, CCOS, CONJG, CABS, AIMAG
      COMPLEX Z, W, VIA
      REAL RVIA
      Z = (0.75, -1.25)
      W = VIA(CSQRT, Z)
      IF (W .NE. CSQRT(Z)) STOP 1
      W = VIA(CEXP, Z)
      IF (W .NE. CEXP(Z)) STOP 2
      W = VIA(CLOG, Z)
      IF (W .NE. CLOG(Z)) STOP 3
      W = VIA(CSIN, Z)
      IF (W .NE. CSIN(Z)) STOP 4
      W = VIA(CCOS, Z)
      IF (W .NE. CCOS(Z)) STOP 5
      W = VIA(CONJG, Z)
      IF (W .NE. (0.75, 1.25)) STOP 6
      IF (RVIA(CABS, Z) .NE. CABS(Z)) STOP 7
      IF (RVIA(AIMAG, Z) .NE. -1.25) STOP 8
      WRITE (6, 100) VIA(CSQRT, Z), VIA(CONJG, Z), RVIA(CABS, Z)
  100 FORMAT (' CX ', 5F10.5)
      STOP 0
      END
      COMPLEX FUNCTION VIA(F, X)
      COMPLEX F, X
      VIA = F(X)
      END
      REAL FUNCTION RVIA(G, X)
      REAL G
      COMPLEX X
      RVIA = G(X)
      END
