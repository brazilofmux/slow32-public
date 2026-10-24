C     A subroutine with an ENTRY is compiled once per entry from its
C     whole text, and what the text declares -- its FORMATs, the labels
C     it ASSIGNs, the names whose address a READ takes -- belongs to the
C     parent.  Compiled for the ENTRY, every FORMAT label was "no FORMAT
C     statement with that label", the ASSIGNed format was lost, and a
C     DOUBLE PRECISION local a READ stores into could lose the value.
      PROGRAM ENTFM1
      COMMON /RES/ NS, NE, DV
      DOUBLE PRECISION DV
      CALL WARD(7)
      CALL HALL(5)
      IF (NS .NE. 5 .OR. NE .NE. 7) STOP 1
      IF (DV .NE. 54.0D0) STOP 2
      STOP 0
      END
      SUBROUTINE HALL(N)
      COMMON /RES/ NS, NE, DV
      DOUBLE PRECISION DV, X, Y
      CHARACTER*12 L
      WRITE (L, 11) N
   11 FORMAT ('H', I4)
      READ (L, '(1X, I4)') NS
      WRITE (6, 11) NS
      RETURN
      ENTRY WARD(N)
      ASSIGN 12 TO K
      WRITE (L, K) N
   12 FORMAT ('W', I4)
      READ (L, '(1X, I4)') NE
      WRITE (6, 12) NE
      OPEN (21, STATUS='SCRATCH', FORM='UNFORMATTED')
      Y = 6.75D0
      WRITE (21) Y
      REWIND 21
      X = 0.0D0
      DO 14 I = 1, N
         READ (21) X
         REWIND 21
         Y = Y + X
   14 CONTINUE
      CLOSE (21)
      DV = Y
      ASSIGN 16 TO J
      GO TO J
      DV = -1.0D0
   16 CONTINUE
      WRITE (6, 13) DV
   13 FORMAT (' DV ', F7.3)
      RETURN
      END
