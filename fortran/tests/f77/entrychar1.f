      FUNCTION C1 (A)
      CHARACTER*4 C1, C2, A, B
      C1 = A
      RETURN
      ENTRY C2 (B)
      C2 = B
      IF (B .EQ. 'wxyz') C2 = 'WXYZ'
      END
      PROGRAM P
      CHARACTER*4 C1, C2
      PRINT *, C1('abcd'), C2('lmno'), C2('wxyz')
      END
