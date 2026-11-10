C     INT of an INTEGER*2 is an INTEGER: a fullword in an unformatted
C     record and twelve columns in a list, where the variable, in
C     parentheses or signed too, is a halfword and seven.
      PROGRAM I2INT1
      INTEGER*2 S, H(3)
      LOGICAL*1 L
      INTEGER K, J, M
      S = 9
      L = .TRUE.
      H(1) = 4
      H(2) = -5
      H(3) = 6
      OPEN (11, FILE='int2int1.dat', FORM='UNFORMATTED')
      WRITE (11) INT(S), INT(H(2))
      WRITE (11) S, H(3), (S), +S
      WRITE (11) (INT(H(K)), K = 1, 3)
      REWIND 11
      READ (11) K, J
      PRINT *, K, J
      READ (11) S, H(1), H(2), H(3)
      PRINT *, S, H
      READ (11) K, J, M
      PRINT *, K, J, M
      CLOSE (11, STATUS='DELETE')
      PRINT *, INT(S), (S), +S, ABS(K), INT(H(1))
      PRINT '(4I6)', INT(S), (S), +S, INT(H(1))
      END
