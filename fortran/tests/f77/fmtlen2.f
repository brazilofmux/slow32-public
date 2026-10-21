C     A dummy CHARACTER array as a format.  Adjustable, F(N), it is the
C     concatenation of its N elements, N taken at run time: with N = 2
C     the two halves of the format are one format, and with N = 1 the
C     first half alone has no closing ), a format error (ERR=, IOSTAT
C     1) though the ) is in the next element of the actual array.
C     Assumed-size, F(*), its size is known nowhere, so it is read to
C     its ) as before.
      PROGRAM FMTLN2
      CHARACTER*4 FA(2)
      INTEGER IOS
      FA(1) = '(I3,'
      FA(2) = 'I3) '
      CALL SHOWA(FA, 2)
      CALL SHOWS(FA)
      CALL CHKA(FA, 1, IOS)
      IF (IOS .NE. 1) STOP 1
      STOP 0
      END
      SUBROUTINE SHOWA(F, N)
      INTEGER N
      CHARACTER*4 F(N)
      WRITE (6, F) 12, 34
      END
      SUBROUTINE SHOWS(F)
      CHARACTER*4 F(*)
      WRITE (6, F) 56, 78
      END
      SUBROUTINE CHKA(F, N, IOS)
      INTEGER N, IOS, J, K
      CHARACTER*4 F(N)
      CHARACTER*8 REC
      REC = ' 11 22  '
      IOS = 0
      READ (REC, F, ERR=10, IOSTAT=IOS) J, K
      IOS = 99
   10 CONTINUE
      END
