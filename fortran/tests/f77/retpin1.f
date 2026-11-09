C     SPDX-License-Identifier: BSD-3-Clause-Open-MPI (DLCTSX ONLY)
C     DLCTSX BELOW IS LAPACK'S TESTING/EIG/DLCTSX.F, REFERENCE LAPACK,
C     UNDER ITS MODIFIED BSD LICENCE (SEE NOTICE).
C     RETURN-VALUE PINNING AGAINST A SYMBOL-FORM STORE (SELFHOST
C     ISSUES-117, ALL FOUR HIR BACKENDS): A LOGICAL FUNCTION SET ITS
C     RESULT IN R1, AND THE STORES TO ITS COMMON MEMBERS FORMED THEIR
C     ADDRESSES IN R1 BEFORE THE RET.  LAPACK'S DLCTSX, THE xGD SUITES.
      LOGICAL FS, DLCTSX, L
      INTEGER M, N, MPLUSN, K, J
      COMMON /MN/ M, N, MPLUSN, K, FS
      M = 1
      N = 1
      MPLUSN = 2
      FS = .TRUE.
      K = 0
      DO 10 J = 1, 4
        L = DLCTSX(1D0, 0D0, 1D0)
        WRITE (*, '(L2, 2I3, L2)') L, K, MPLUSN, FS
   10 CONTINUE
      END
      LOGICAL          FUNCTION DLCTSX( AR, AI, BETA )
      DOUBLE PRECISION   AI, AR, BETA
      LOGICAL            FS
      INTEGER            I, M, MPLUSN, N
      COMMON             / MN / M, N, MPLUSN, I, FS
      SAVE
      IF( FS ) THEN
         I = I + 1
         IF( I.LE.M ) THEN
            DLCTSX = .FALSE.
         ELSE
            DLCTSX = .TRUE.
         END IF
         IF( I.EQ.MPLUSN ) THEN
            FS = .FALSE.
            I = 0
         END IF
      ELSE
         I = I + 1
         IF( I.LE.N ) THEN
            DLCTSX = .TRUE.
         ELSE
            DLCTSX = .FALSE.
         END IF
         IF( I.EQ.MPLUSN ) THEN
            FS = .TRUE.
            I = 0
         END IF
      END IF
      RETURN
      END
