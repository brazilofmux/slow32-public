C     NAMELIST of INTEGER*2 and LOGICAL*1: each element's storage is its
C     declared width (a halfword, a byte), not a fullword INTEGER/LOGICAL.
C     Three halfwords that were stored as INTEGER would overlap; a byte
C     LOGICAL that was stored as INTEGER would clobber the next word.
      PROGRAM NMLI2
      INTEGER*2 H(3)
      LOGICAL*1 Q, R
      NAMELIST /BOX/ H, Q, R
      H(1) = 1
      H(2) = 2
      H(3) = 3
      Q = .TRUE.
      R = .FALSE.
      WRITE (12, BOX)
      REWIND 12
      H(1) = 0
      H(2) = 0
      H(3) = 0
      Q = .FALSE.
      R = .TRUE.
      READ (12, BOX)
      IF (H(1).NE.1) STOP 1
      IF (H(2).NE.2) STOP 2
      IF (H(3).NE.3) STOP 3
      IF (.NOT.Q) STOP 4
      IF (R) STOP 5
      WRITE (6, 101) H, Q, R
  101 FORMAT (1X, 3I4, 2L2)
      STOP
      END
