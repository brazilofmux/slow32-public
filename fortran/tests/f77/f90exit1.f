C     FORTRAN 90 EXIT AND CYCLE, OF THE INNERMOST DO: COUNTED (LABELLED AND
C     END DO), DO WHILE, NESTED, TWO DOS SHARING A TERMINAL LABEL, AS THE
C     TAIL OF A LOGICAL IF; AND RECURSIVE SUBROUTINES, THE HEADER THE
C     ONLY CHANGE.  (A RECURSIVE FUNCTION REFERRING TO ITSELF BY NAME NEEDS
C     RESULT, AND IS REFUSED UNTIL IT EXISTS.)
      PROGRAM F90EX1
      INTEGER I, J, N, K, ISUM
      N = 0
      DO 10 I = 1, 10
        IF (MOD(I, 2) == 0) CYCLE
        IF (I > 7) EXIT
        N = N + I
   10 CONTINUE
      WRITE (*, 100) N, I
      N = 0
      DO I = 1, 5
        DO J = 1, 5
          IF (J > I) EXIT
          IF (J == 2) CYCLE
          N = N + 10 * I + J
        END DO
      END DO
      WRITE (*, 100) N, I
      K = 0
      DO WHILE (K < 100)
        K = K + 7
        IF (MOD(K, 3) == 0) CYCLE
        IF (K > 50) EXIT
      END DO
      WRITE (*, 100) K, 0
      N = 0
      DO 20 I = 1, 4
      DO 20 J = 1, 4
        IF (J == I) CYCLE
        N = N + J
   20 CONTINUE
      WRITE (*, 100) N, J
      CALL KFACT(6, I)
      CALL KFIB(12, J)
      WRITE (*, 100) I, J
      CALL HANOI(5, ISUM)
      WRITE (*, 100) ISUM, 0
  100 FORMAT (1X, 2I8)
      END
      RECURSIVE SUBROUTINE KFACT(N, M)
      INTEGER N, M
      IF (N <= 1) THEN
        M = 1
      ELSE
        CALL KFACT(N - 1, M)
        M = N * M
      END IF
      END
      RECURSIVE SUBROUTINE KFIB(N, M)
      INTEGER N, M, A, B
      IF (N < 2) THEN
        M = N
      ELSE
        CALL KFIB(N - 1, A)
        CALL KFIB(N - 2, B)
        M = A + B
      END IF
      END
      RECURSIVE SUBROUTINE HANOI(N, M)
      INTEGER N, M, M1, M2
      IF (N == 0) THEN
        M = 0
        RETURN
      END IF
      CALL HANOI(N - 1, M1)
      CALL HANOI(N - 1, M2)
      M = M1 + M2 + 1
      END
