C     Formats given at run time: blanks in a format are not significant
C     outside its strings (a width or a count may be split by one), a
C     format kept in a CHARACTER variable or built in an array, a quoted
C     string inside it, and reversion to the last open parenthesis.
      PROGRAM FMTRUN
      CHARACTER*20 F1, F2
      CHARACTER*4 PART(3)
      INTEGER K(6)
      REAL X(2)
      DO 5 I = 1, 6
    5 K(I) = I * 7
      X(1) = 3.5
      X(2) = -0.125
      F1 = '( 3I 4)'
      WRITE (6, F1) K(1), K(2), K(3)
      F1 = '(1X, 2 F8. 3)'
      WRITE (6, F1) X
      F2 = '(''TOTAL:'', I 5)'
      WRITE (6, F2) K(6)
      PART(1) = '(1X,'
      PART(2) = 'I3, '
      PART(3) = ' 2X)'
      WRITE (6, PART) K
      F1 = '(1X, I2, ( I 3))'
      WRITE (6, F1) K
      END
