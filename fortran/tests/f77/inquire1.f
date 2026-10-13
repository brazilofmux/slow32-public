C     INQUIRE with the unit given without UNIT= (12.10.3): as a constant
C     and as a variable, beside the keyword form.  The front end had
C     taken only keyword=value items.
      INTEGER IU, N, K
      LOGICAL L
      IU = 21
      OPEN (21, ACCESS='DIRECT', RECL=8, STATUS='SCRATCH')
      WRITE (21, REC=1) 11
      WRITE (21, REC=2) 22
      INQUIRE (21, NEXTREC=N)
      PRINT *, 'NEXTREC', N
      INQUIRE (IU, OPENED=L, NUMBER=K)
      PRINT *, 'OPENED', L, K
      INQUIRE (UNIT=IU, NEXTREC=N)
      PRINT *, 'KEYWORD', N
      CLOSE (21)
      INQUIRE (IU, OPENED=L)
      PRINT *, 'CLOSED', L
      END
