C     ** Whole-array I/O of an adjustable array.  F77 12.8.2 lets an
C     array name in an I/O list stand for all its elements in storage
C     order, an adjustable dummy's too; it was refused ("whole-array
C     I/O needs constant extents").  Formatted, list-directed READ,
C     unformatted, a lower bound other than 1, and CHARACTER*(*).  From
C     fort370.  Self-checking; success reaches STOP 0.
      PROGRAM WHADJ1
      REAL X(2,3)
      INTEGER K(-1:3)
      CHARACTER*4 C(3)
      DATA X /1.5, 2.5, 3.5, 4.5, 5.5, 6.5/
      DATA K /21, 22, 23, 24, 25/
      DATA C /'fig', 'kiwi', 'yam'/
      CALL SHOW(X, 2, 3, K, -1, 3, C, 3)
      IF (X(2,3) .NE. 9.0) STOP 1
      IF (K(-1) .NE. 5 .OR. K(3) .NE. 1) STOP 2
      STOP 0
      END
C
      SUBROUTINE SHOW(A, M, N, J, LO, HI, S, NS)
      INTEGER M, N, LO, HI, NS
      REAL A(M, N)
      INTEGER J(LO:HI)
      CHARACTER*(*) S(NS)
      CHARACTER*40 T
      WRITE (6, 100) A
      WRITE (6, 101) J
      WRITE (6, 102) S
      T = '5 4 3 2 1'
      READ (T, *) J
      WRITE (6, 101) J
      T = ' 4.0 5.0 6.0 7.0 8.0 9.0'
      READ (T, '(6F4.1)') A
      WRITE (6, 100) A
      OPEN (2, FORM='UNFORMATTED', STATUS='SCRATCH')
      WRITE (2) A, J
      REWIND 2
      A(1, 1) = -1.0
      J(HI) = -1
      READ (2) A, J
      CLOSE (2)
      IF (A(1, 1) .NE. 4.0 .OR. J(HI) .NE. 1) STOP 3
      WRITE (6, 100) A
      WRITE (6, 101) J
  100 FORMAT (1X, 'A', 6F6.1)
  101 FORMAT (1X, 'J', 5I4)
  102 FORMAT (1X, 'S', 3(1X, A))
      END
