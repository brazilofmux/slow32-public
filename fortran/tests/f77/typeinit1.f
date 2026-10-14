C     ** Initial values in a type statement (IBM's FORTRAN IV and VS
C     FORTRAN extension, gfortran too): the same as DATA for the whole
C     entity, repeat counts included.  They had been skipped, and with
C     them every name after them in the statement -- K was undefined
C     and ARR lost its dimension, so ARR(1)=... read as a statement
C     function.  Found by fort370 in a program from H390-MVS.
C     Self-checking; success reaches STOP 0.
      INTEGER K/12345/, ARR(3)/1,2*7/, M
      INTEGER DIGIT(3)/1H0,1H1,1H2/
      CHARACTER*4 C/'ABCD'/
      REAL X(2)/1.5,2.5/
      ARR(1)=ARR(1)+0
      M=1
      IF (K .NE. 12345) STOP 1
      IF (ARR(1).NE.1 .OR. ARR(2).NE.7 .OR. ARR(3).NE.7) STOP 2
      IF (C .NE. 'ABCD') STOP 3
      IF (X(1).NE.1.5 .OR. X(2).NE.2.5) STOP 4
      IF (DIGIT(2) .EQ. DIGIT(1)) STOP 5
      WRITE(6,10) K, ARR, M, C, X
10    FORMAT(1X,'TYPEINIT',I6,3I3,I2,1X,A4,2F5.1)
      STOP 0
      END
