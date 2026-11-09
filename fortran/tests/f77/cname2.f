C     CONSTRUCT NAMES IN FIXED FORM (FORTRAN 90), WHERE BLANKS DO NOT
C     COUNT: ELSE IFX IS THE ELSE OF THE CONSTRUCT NAMED IFX, NOT AN
C     ELSE IF; END DO NAME ON A LABELLED DO; THENX IS THEN AND NAME X.
      PROGRAM CNAME2
      INTEGER I, J, K
      K = 0
      DO 30 I = 1, 4
      IFX : IF (I .EQ. 1) THEN
         K = K + 1
      ELSE IF (I .EQ. 2) THEN IFX
         K = K + 10
      ELSE IFX
         K = K + 100
      END IF IFX
   30 CONTINUE
      PRINT *, 'IFX', K
      K = 0
      R O W : DO 20 I = 1, 3
         C O L : DO 10 J = 1, 3
            IF (J .GT. I) CYCLE ROW
            IF (I .EQ. 3 .AND. J .EQ. 2) EXIT R O W
            K = K + 10 * I + J
   10    END DO COL
   20 END DO ROW
      PRINT *, 'ROW', K, I, J
      K = 0
      X: DO
         K = K + 3
         IF (K .GT. 10) EXIT X
      ENDDO X
      PRINT *, 'X', K
      END
