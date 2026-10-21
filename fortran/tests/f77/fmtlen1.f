C     A CHARACTER format is walked only for its declared length.  F is
C     four characters '(I5' with no closing ); the ) is in G, which
C     COMMON places right after it.  Walked past its length, F would
C     read on into G, find a valid (I5 ), and read the 42: that is
C     STOP 1.  Bounded, it is a format error: ERR= with IOSTAT 1.
      PROGRAM FMTLEN
      CHARACTER*4 F, G
      CHARACTER*8 REC
      INTEGER K, IOS
      COMMON /FMTS/ F, G
      F = '(I5'
      G = ' )  '
      REC = '   42   '
      IOS = 0
      K = 0
      READ (REC, F, ERR=10, IOSTAT=IOS) K
      STOP 1
   10 IF (IOS .NE. 1) STOP 2
      IF (K .NE. 0) STOP 3
      STOP 0
      END
