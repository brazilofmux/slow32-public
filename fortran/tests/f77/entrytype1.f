C     An ENTRY of another type than its FUNCTION, in Fortran 77: the
C     entry returns a REAL and the function is still an INTEGER.
      INTEGER FUNCTION NDAYS(M)
      INTEGER M
      REAL WEEKS
      NDAYS = 30*M + 5
      RETURN
      ENTRY WEEKS(M)
      WEEKS = (30*M + 5)/7.0
      END
      PROGRAM ETYPE
      INTEGER NDAYS
      REAL WEEKS
      PRINT *, NDAYS(2), WEEKS(2)
      END
