      INTEGER I, J
      OPEN (10, FILE="ef1.dat", STATUS="REPLACE")
      DO 5 I = 1, 20
 5    WRITE (10, "(I4)") I
      REWIND 10
      DO 6 I = 1, 10
 6    READ (10, "(I4)") J
      ENDFILE 10
      CLOSE (10)
      OPEN (10, FILE="ef1.dat", STATUS="OLD")
      DO 7 I = 1, 20
 7    READ (10, "(I4)", END=9) J
      PRINT *, "no end"
 9    PRINT *, J
      CLOSE (10, STATUS="DELETE")
      END
