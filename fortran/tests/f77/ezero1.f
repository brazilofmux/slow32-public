C     Zero under E and D editing: its exponent is zero whatever the
C     scale factor or the number of digits.  1PE of zero printed E-01
C     here, and a zero with two or more digits E+01.
      PROGRAM EZERO1
      DOUBLE PRECISION ZIP, ONEH
      REAL ZR
      ZIP = 0.0D0
      ONEH = 2.75D0
      ZR = 0.0
      WRITE (6, 10) ZIP, ONEH, ZIP, ONEH
      WRITE (6, 11) ZIP, ONEH, ZIP
      WRITE (6, 12) ZIP, ZR, ZIP
      WRITE (6, 13) ZR, ZR
   10 FORMAT (1X, 1PE11.3, E11.3, 0PE11.3, E11.3)
   11 FORMAT (1X, 3PE13.4, -2PE13.4, 0PE9.1)
   12 FORMAT (1X, 2PD15.6, E13.4E3, 1PE12.4E1)
   13 FORMAT (1X, E12.5, F8.3)
      STOP 0
      END
