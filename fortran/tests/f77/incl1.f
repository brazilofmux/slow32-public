C     INCLUDE (MIL-STD-1753 2.3) in its three spellings: quoted, in
C     parentheses and bare; nested; in a main program, a subroutine and
C     a function.  Pinned by incl1.expected, since gfortran takes only
C     the quoted spelling and none of the file-name forms here.
      PROGRAM INCL1
      INCLUDE 'incl1a.inc'
      KSUM = 0
      CALL ADDEM(4)
      INCLUDE (INCL1B)
      IF (KSUM .NE. 4 + 3 * KBASE) STOP 1
      IF (NTWICE(KBASE) .NE. 2 * KBASE + NEXTRA) STOP 2
      WRITE (6, 100) KSUM, NTWICE(KBASE)
  100 FORMAT (' INCL ', 2I6)
      STOP 0
      END
      SUBROUTINE ADDEM(N)
      INCLUDE 'incl1a.inc'
      KSUM = KSUM + N
      END
      INTEGER FUNCTION NTWICE(K)
      INCLUDE incl1a
      NTWICE = 2 * K + NEXTRA
      END
