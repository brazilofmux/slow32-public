C     A formatted record holding CHAR(0) is written to its full length:
C     the record writer stopped at the first zero byte, losing the rest
C     of the line, and a direct-access record came out short of RECL.
      PROGRAM NULREC
      CHARACTER*12 R, B
      R = 'PQ      TUVW'
      R(3:5) = CHAR(0) // CHAR(0) // CHAR(0)
      OPEN (8, FILE='nulrec1.tmp', STATUS='UNKNOWN')
      WRITE (8, '(A)') R
      WRITE (8, '(A)') 'NEXTLINE'
      CLOSE (8)
      OPEN (8, FILE='nulrec1.tmp', STATUS='OLD')
      READ (8, '(A)') B
      IF (B(9:12) .NE. 'TUVW') STOP 1
      IF (ICHAR(B(4:4)) .NE. 0) STOP 2
      READ (8, '(A)') B
      IF (B(1:8) .NE. 'NEXTLINE') STOP 3
      CLOSE (8, STATUS='DELETE')
      OPEN (9, FILE='nulrec1.dat', ACCESS='DIRECT', FORM='FORMATTED',
     1      RECL=12, STATUS='UNKNOWN')
      WRITE (9, '(A)', REC=1) R
      WRITE (9, '(A)', REC=2) 'SECOND REC12'
      READ (9, '(A)', REC=2) B
      IF (B .NE. 'SECOND REC12') STOP 4
      READ (9, '(A)', REC=1) B
      IF (B(9:12) .NE. 'TUVW') STOP 5
      CLOSE (9, STATUS='DELETE')
      WRITE (6, '(A)') ' RECORDS WHOLE'
      STOP 0
      END
