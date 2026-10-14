C     ** A local scalar in static storage (DATA or SAVE) that the unit
C     never stores cannot change, so its load moves out of a loop (LEN
C     below).  Every other way the unit can change one must keep the
C     load in the loop: passed to a subroutine, stored through an
C     EQUIVALENCE, read into, an implied-DO index, a DO variable.
C     From fort370.  Self-checking; success reaches STOP 0.
      INTEGER LEN/4/, KC/1/, KE/1/, KR/1/, KI/1/, KD/1/
      INTEGER ME, I, J, S, SC, SE, SR, SI, SD
      CHARACTER*2 C
      EQUIVALENCE (KE, ME)
      SAVE KI, KD
      S = 0
      SC = 0
      SE = 0
      SR = 0
      SI = 0
      SD = 0
      DO 10 I = 1, LEN
        S = S + LEN
        SC = SC + KC
        CALL INC (KC)
        SE = SE + KE
        ME = ME + 2
        SR = SR + KR
        WRITE (C, '(I2)') I + 10
        READ (C, '(I2)') KR
        SI = SI + KI
        WRITE (C, '(2I1)') (KI, KI = 1, 2)
   10 CONTINUE
      DO 20 KD = 1, 3
        DO 20 J = 1, 2
          SD = SD + KD
   20 CONTINUE
      WRITE (6,100) S, SC, SE, SR, SI, SD
  100 FORMAT (1X,'>LDRO',6I5)
      IF (S .NE. 16) STOP 1
      IF (SC .NE. 10) STOP 2
      IF (SE .NE. 16) STOP 3
      IF (SR .NE. 37) STOP 4
      IF (SI .NE. 10) STOP 5
      IF (SD .NE. 12) STOP 6
      STOP 0
      END
      SUBROUTINE INC (K)
      INTEGER K
      K = K + 1
      END
