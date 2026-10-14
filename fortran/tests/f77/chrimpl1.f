C     ** An implicitly typed CHARACTER gets its full length at first
C     sight: IMPLICIT CHARACTER*14 (C) had given CVCOMP a one-byte slot,
C     overlapping the variables after it (FCVS FM301 test 18 on S/370;
C     here it overlapped too, and the compare came out wrong).  Found
C     by fort370.  Self-checking; success reaches STOP 0.
      IMPLICIT CHARACTER*14 (C)
      IMPLICIT CHARACTER*2 (F)
      CHARACTER FVTN01*3
      CVCOMP = '   '
      FVTN01 = 'ABC'
      CVCORR = 'ABC'
      CVCOMP = FVTN01
      IF (CVCOMP .NE. 'ABC') STOP 1
      IF (CVCORR .NE. 'ABC') STOP 2
      IF (FVTN01 .NE. 'ABC') STOP 3
      WRITE(6,101) CVCOMP, FVTN01, CVCORR
  101 FORMAT(' CHRIMPL ',A14,'#',A3,'#',A14,'#')
      STOP 0
      END
