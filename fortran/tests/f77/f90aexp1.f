C     FORTRAN 90 ARRAY ASSIGNMENT: A WHOLE ARRAY OR A SECTION ASSIGNED A
C     SCALAR OR AN ARRAY EXPRESSION -- ELEMENTAL ARITHMETIC AND INTRINSICS
C     (ABS, DCONJG), A REDUCTION INSIDE ONE, 2-D AND STRIDED SECTIONS, AND
C     THE LEFT SIDE'S ARRAY ON THE RIGHT (A(2:5) = A(1:4); LAPACK'S
C     QA(KW:KB,KW-1) = QA(KW,KW-1)*DCONJG(QC(1,1:N))), WHICH NEEDS THE WHOLE
C     RIGHT SIDE BEFORE ANY ELEMENT IS STORED.
      PROGRAM F90AX1
      DOUBLE PRECISION A(5), B(5), C(5), M(3,4), H(2,3), Z(6,3)
      COMPLEX*16 QA(4,4), QC(2,4)
      REAL R(6)
      INTEGER K(6), I, J, KW, KB
      DO 10 I = 1, 5
        B(I) = DBLE(I)
        C(I) = DBLE(10 - I)
   10 CONTINUE
      A = 0
      WRITE (*, 100) A
      A = B + C * 2.0D0
      WRITE (*, 100) A
      A = ABS(B - C) + MAXVAL(B)
      WRITE (*, 100) A
      A(2:5) = A(1:4)
      WRITE (*, 100) A
      DO 20 J = 1, 4
      DO 20 I = 1, 3
        M(I,J) = 10 * I + J
   20 CONTINUE
      H = M(2:3, 2:4)
      WRITE (*, 100) H
      M(1:3:2, 4) = M(1:3:2, 1) * 0.5D0
      WRITE (*, 100) M
      DO 30 J = 1, 3
      DO 30 I = 1, 6
        Z(I,J) = I - J
   30 CONTINUE
      Z(2:4, 3) = Z(2:4, 3) + Z(2:4, 2) * 10
      WRITE (*, 100) Z(1:6, 3)
      DO 40 J = 1, 4
      DO 40 I = 1, 4
        QA(I,J) = DCMPLX(DBLE(I), DBLE(J))
   40 CONTINUE
      DO 50 J = 1, 4
        QC(1,J) = DCMPLX(0.5D0 * J, -1.0D0)
   50 CONTINUE
      KW = 2
      KB = 4
      QA(KW:KB, KW-1) = QA(KW, KW-1) * DCONJG(QC(1, 1:KB-KW+1))
      WRITE (*, 200) (QA(I,1), I = 1, 4)
      K = 3
      K(2:6:2) = K(1:5:2) + 1
      WRITE (*, 300) K
      R = 1.5
      R(4:6) = -R(1:3) * 2
      WRITE (*, 100) R
  100 FORMAT (1X, 6F9.2)
  200 FORMAT (1X, 4F9.2)
  300 FORMAT (1X, 6I5)
      END
