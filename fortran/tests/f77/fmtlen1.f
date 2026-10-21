C     A CHARACTER format is walked only for its declared length.  F is
C     four characters '(I5' with no closing ); the ) lives in G.  Without
C     the length the runtime would read on and take a valid format.
C     ERR=/IOSTAT= see the format error; STOP 0 is the assertion.
      PROGRAM FMTLEN
      CHARACTER*4 F
      CHARACTER*4 G
      INTEGER K, IOS
      F = '(I5'
      G = ' )  '
      IOS = 0
      K = 0
      READ (5, F, ERR=10, IOSTAT=IOS) K
      STOP 1
   10 IF (IOS .NE. 1) STOP 2
      STOP 0
      END
