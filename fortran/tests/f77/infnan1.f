C     INFINITY AND NAN UNDER F, E, D, G, ES AND EN, IN FIELDS OF EVERY
C     WIDTH THAT MATTERS: "Infinity" PAST 8 COLUMNS, "Inf" UNDER, THE SIGN
C     BEFORE IT, "NaN" UNSIGNED, ASTERISKS WHERE IT DOES NOT FIT -- AS
C     GFORTRAN WRITES THEM, LIST-DIRECTED TOO.  E12.3 HAD GIVEN
C     0.INFE+01 (LAPACK'S SBAL SUITE), AND LIST-DIRECTED INF AND NAN.
      PROGRAM INFNAN
      DOUBLE PRECISION P, M, Q, Z
      REAL R
      Z = 0
      P = 1 / Z
      M = -P
      Q = Z / Z
      R = REAL(P)
      WRITE (*, '(3E12.3)') P, M, Q
      WRITE (*, '(3F9.2)') P, M, Q
      WRITE (*, '(3D8.1)') P, M, Q
      WRITE (*, '(3G4.1)') P, M, Q
      WRITE (*, '(3ES10.2, 3EN3.1)') P, M, Q, P, M, Q
      WRITE (*, '(SP, 2E9.2, SS, E2.1, E3.1)') P, M, P, Q
      WRITE (*, '(E12.3, F3.0)') R, R
      WRITE (*, *) P, M, Q, R
      END
