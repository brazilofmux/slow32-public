C     ** The main program may follow its subprograms.  The unit scan
C     began a main program only before the first subprogram, so one
C     after them was taken for more of the last subprogram, past its
C     END, and never compiled: no message, and the link failed for want
C     of main.  Found by fort370, which carries this scan for S/370.
C     A BLOCK DATA, a FUNCTION and a SUBROUTINE come first; the main
C     program, with its PROGRAM card, comes last.  Self-checking;
C     success reaches STOP 0.
      BLOCK DATA BD
      COMMON /C/ K
      DATA K/41/
      END
      INTEGER FUNCTION NEXT(N)
      NEXT = N + 1
      END
      SUBROUTINE BUMP
      COMMON /C/ K
      K = NEXT(K)
      END
      PROGRAM MAINLS
      COMMON /C/ K
      CALL BUMP
      IF (K .NE. 42) STOP 1
      WRITE (6, 100) K
100   FORMAT (1X, 'MAIN LAST', I4)
      STOP 0
      END
