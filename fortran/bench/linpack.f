C     LINPACK benchmark kernel, sized to dominate startup cost.
C     Factors and solves an N x N system REPS times.  Self-checking:
C     exits 0 only if every solution component is 1 to within 1e-8.
      PROGRAM LINBEN
      INTEGER LDA, N, REPS, IPVT(64), INFO, I, J, R
      DOUBLE PRECISION A(64,64), AA(64,64), B(64), RESID
      LDA = 64
      N = 64
      REPS = 400
      DO 20 J = 1, N
         DO 10 I = 1, N
            AA(I,J) = 1.0D0 / DBLE(I + J)
   10    CONTINUE
         AA(J,J) = AA(J,J) + DBLE(N)
   20 CONTINUE
      DO 70 R = 1, REPS
         DO 40 J = 1, N
            DO 30 I = 1, N
               A(I,J) = AA(I,J)
   30       CONTINUE
   40    CONTINUE
         DO 60 I = 1, N
            B(I) = 0.0D0
            DO 50 J = 1, N
               B(I) = B(I) + AA(I,J)
   50       CONTINUE
   60    CONTINUE
         CALL DGEFA(A, LDA, N, IPVT, INFO)
         IF (INFO .NE. 0) STOP 91
         CALL DGESL(A, LDA, N, IPVT, B)
   70 CONTINUE
      RESID = 0.0D0
      DO 80 I = 1, N
         RESID = DMAX1(RESID, DABS(B(I) - 1.0D0))
   80 CONTINUE
      IF (RESID .GT. 1.0D-8) STOP 92
      STOP 0
      END

      SUBROUTINE DAXPY(N, DA, DX, INCX, DY, INCY)
      DOUBLE PRECISION DX(1), DY(1), DA
      INTEGER I, INCX, INCY, N
      IF (N .LE. 0) RETURN
      IF (DA .EQ. 0.0D0) RETURN
      DO 10 I = 1, N
         DY(I) = DY(I) + DA*DX(I)
   10 CONTINUE
      RETURN
      END

      SUBROUTINE DSCAL(N, DA, DX, INCX)
      DOUBLE PRECISION DA, DX(1)
      INTEGER I, INCX, N
      IF (N .LE. 0) RETURN
      DO 10 I = 1, N
         DX(I) = DA*DX(I)
   10 CONTINUE
      RETURN
      END

      INTEGER FUNCTION IDAMAX(N, DX, INCX)
      DOUBLE PRECISION DX(1), DMAX
      INTEGER I, INCX, N
      IDAMAX = 0
      IF (N .LT. 1) RETURN
      IDAMAX = 1
      IF (N .EQ. 1) RETURN
      DMAX = DABS(DX(1))
      DO 10 I = 2, N
         IF (DABS(DX(I)) .LE. DMAX) GO TO 10
         IDAMAX = I
         DMAX = DABS(DX(I))
   10 CONTINUE
      RETURN
      END

      SUBROUTINE DGEFA(A, LDA, N, IPVT, INFO)
      INTEGER LDA, N, IPVT(1), INFO
      DOUBLE PRECISION A(LDA,1), T
      INTEGER IDAMAX, J, K, KP1, L, NM1
      INFO = 0
      NM1 = N - 1
      IF (NM1 .LT. 1) GO TO 70
      DO 60 K = 1, NM1
         KP1 = K + 1
         L = IDAMAX(N-K+1, A(K,K), 1) + K - 1
         IPVT(K) = L
         IF (A(L,K) .EQ. 0.0D0) GO TO 40
         IF (L .EQ. K) GO TO 10
         T = A(L,K)
         A(L,K) = A(K,K)
         A(K,K) = T
   10    CONTINUE
         T = -1.0D0/A(K,K)
         CALL DSCAL(N-K, T, A(K+1,K), 1)
         DO 30 J = KP1, N
            T = A(L,J)
            IF (L .EQ. K) GO TO 20
            A(L,J) = A(K,J)
            A(K,J) = T
   20       CONTINUE
            CALL DAXPY(N-K, T, A(K+1,K), 1, A(K+1,J), 1)
   30    CONTINUE
         GO TO 50
   40    CONTINUE
         INFO = K
   50    CONTINUE
   60 CONTINUE
   70 CONTINUE
      IPVT(N) = N
      IF (A(N,N) .EQ. 0.0D0) INFO = N
      RETURN
      END

      SUBROUTINE DGESL(A, LDA, N, IPVT, B)
      INTEGER LDA, N, IPVT(1)
      DOUBLE PRECISION A(LDA,1), B(1), T
      INTEGER K, KB, L, NM1
      NM1 = N - 1
      IF (NM1 .LT. 1) GO TO 30
      DO 20 K = 1, NM1
         L = IPVT(K)
         T = B(L)
         IF (L .EQ. K) GO TO 10
         B(L) = B(K)
         B(K) = T
   10    CONTINUE
         CALL DAXPY(N-K, T, A(K+1,K), 1, B(K+1), 1)
   20 CONTINUE
   30 CONTINUE
      DO 40 KB = 1, N
         K = N + 1 - KB
         B(K) = B(K)/A(K,K)
         T = -B(K)
         CALL DAXPY(K-1, T, A(1,K), 1, B(1), 1)
   40 CONTINUE
      RETURN
      END
