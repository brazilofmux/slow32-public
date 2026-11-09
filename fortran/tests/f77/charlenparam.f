C     A CHARACTER length that names a PARAMETER, inside a FUNCTION: the
C     result's and a local's.  The unit scan reads a FUNCTION's type
C     statements before anything is compiled, and a name in a length
C     there crashed the compiler.
      PROGRAM CLP
      CHARACTER*4 F
      CHARACTER*6 G
      INTEGER H
      PRINT *, '[', F(1), '][', G(2), ']', H(3)
      END
      FUNCTION F(K)
      PARAMETER (N=4)
      CHARACTER*(N) F
      CHARACTER*(N+2) T
      T = 'ABCDEFGH'
      F = T(K+1:)
      END
      CHARACTER*(*) FUNCTION G(K)
      PARAMETER (N=4, M=N/2)
      CHARACTER*(N+M) T
      T = 'abcdefgh'
      G = T(K:)
      END
      INTEGER FUNCTION H(K)
      INTEGER N
      REAL X
      PARAMETER (X=2.5)
      PARAMETER (N=4)
      CHARACTER*(N+2) T, U*(N)
      T = 'ABCDEFGH'
      U = T
      H = ICHAR(T(K:K)) + LEN(T) + LEN(U) + INT(X)
      END
