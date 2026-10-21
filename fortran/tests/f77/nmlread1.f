C     NAMELIST READ: records in the syntax every compiler takes --
C     &GROUP, NAME=value, NAME(i)=, r*value, a quoted string with its
C     quote doubled, a name left out, another group skipped, / to end --
C     and the values read printed by FORMAT.
      PROGRAM NMLRD
      INTEGER M, J(5)
      REAL W, V(4)
      DOUBLE PRECISION E
      LOGICAL F
      CHARACTER*8 S
      NAMELIST /TOOLS/ M, J, W, V, E, F, S
      M = 1
      W = 2.0
      E = 3.0D0
      F = .FALSE.
      S = 'NONE'
      DO 5 I = 1, 5
    5 J(I) = I
      DO 6 I = 1, 4
    6 V(I) = 0.0
      WRITE (9, 101) ' &SPARE M=77 /'
      WRITE (9, 101) ' &TOOLS M=12, J(2)=40, J(4)=-9, W=0.5,'
      WRITE (9, 101) '  V=3*2.25, E=1.25D-2, F=.TRUE.,'
      WRITE (9, 101) '  S=''RASP''''S'' /'
      WRITE (9, 101) ' &TOOLS W=-8.0, V(4)=6.5 /'
      REWIND 9
      READ (9, TOOLS)
      WRITE (6, 102) M, J, W, V, E, F, S
      READ (9, TOOLS)
      WRITE (6, 102) M, J, W, V, E, F, S
      READ (9, TOOLS, END=30)
      WRITE (6, 103) 'NO END'
      GO TO 40
   30 WRITE (6, 103) 'END'
   40 CONTINUE
  101 FORMAT (A)
  102 FORMAT (1X, I4, 5I4, F7.2, 4F6.2, F9.5, L2, 1X, A)
  103 FORMAT (1X, A)
      END
