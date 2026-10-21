C     NAMELIST WRITE in FORTRAN H's form: &GROUP, NAME=value, records
C     that begin with a blank and never split a value, INTEGER I11, REAL
C     G15.8, DOUBLE PRECISION G23.16 with D, zero as 0.0, COMPLEX in
C     parentheses, T and F, and &END.  Other compilers write NAMELIST
C     their own way, so the expected output is H's.
      PROGRAM NMLWR
      INTEGER IVAL, ICOL(12)
      REAL RVAL, ZERO
      DOUBLE PRECISION DVAL
      COMPLEX CVAL
      LOGICAL LT, LF
      CHARACTER*5 WORD
      NAMELIST /PANEL/ IVAL, RVAL, ZERO, DVAL, CVAL, LT, LF, WORD, ICOL
      IVAL = 271828
      RVAL = -12.5
      ZERO = 0.0
      DVAL = 1.0D-7
      CVAL = (4.0, -0.25)
      LT = .TRUE.
      LF = .FALSE.
      WORD = 'FERN'
      DO 5 I = 1, 12
    5 ICOL(I) = I * 1111
      WRITE (6, PANEL)
      END
