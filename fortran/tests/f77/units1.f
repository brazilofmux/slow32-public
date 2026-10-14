C     ** Units are 0-99: an old program's files on units 55 and 66
C     were refused as beyond unit 31.  Unit 99 is OPENed, written,
C     rewound and read back; unit 0 is preconnected (stderr, as
C     gfortran has it).  Self-checking; success reaches STOP 0.
      INTEGER I, J, K
      OPEN (99, FILE='units1.dat', STATUS='UNKNOWN')
      DO 10 I = 1, 3
         WRITE (99, 100) I, I*I
   10 CONTINUE
  100 FORMAT (2I5)
      REWIND 99
      K = 0
      DO 20 I = 1, 3
         READ (99, 100) J, K
         IF (J .NE. I .OR. K .NE. I*I) STOP 1
   20 CONTINUE
      CLOSE (99, STATUS='DELETE')
      WRITE (0, 200)
  200 FORMAT (' UNIT 0')
      WRITE (6, 300) J, K
  300 FORMAT (1X, '>UNIT99', 2I5)
      STOP 0
      END
