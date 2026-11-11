      INTEGER I(4), K
      DATA I / 4 * 0 /
      OPEN(11,FILE='l1tmp.dat',FORM='FORMATTED',STATUS='UNKNOWN')
      WRITE(11,'("1 2 3 4 5 6 7 8 9")')
      WRITE(11,'("1 2 3 4 5 6 7 8 9")')
      CLOSE(11)
      OPEN(11,FILE='l1tmp.dat',FORM='FORMATTED')
      WRITE(11,'("9 8 7 6")')
      CLOSE(11)
      OPEN(11,FILE='l1tmp.dat',FORM='FORMATTED')
      READ(11,*) I
      PRINT *, I
      READ(11,*, END=200) I
      PRINT *, 'more', I
 200  CONTINUE
      REWIND 11
      WRITE(11,'("5 5 5 5")')
      WRITE(11,'("6 6 6 6")')
      BACKSPACE 11
      WRITE(11,'("7 7 7 7")')
      REWIND 11
      K = 0
 10   READ(11,*,END=20) I
      K = K + 1
      PRINT *, K, I
      GO TO 10
 20   CLOSE(11,STATUS='DELETE')
      OPEN(12,FILE='l1tmp.unf',FORM='UNFORMATTED')
      WRITE(12) 1, 2, 3
      WRITE(12) 4, 5, 6
      WRITE(12) 7, 8, 9
      REWIND 12
      WRITE(12) 10
      REWIND 12
      K = 0
 30   READ(12,END=40) I(1)
      K = K + 1
      PRINT *, K, I(1)
      GO TO 30
 40   CLOSE(12,STATUS='DELETE')
      END
