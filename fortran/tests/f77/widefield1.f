C     Fields wider than the usual: the runtime's digit buffers were 32
C     to 64 characters, and E199.192 wrote past them.
      CHARACTER*200 T
      CHARACTER*60 S
      WRITE (T, '(E199.192,A)') 1.5, 'z'
      PRINT *, T(1:12), T(190:200)
      WRITE (S, '(I50.40)') -12345
      PRINT *, S
      WRITE (S, '(B50.40)') 5
      PRINT *, S
      WRITE (S, '(E60.45)') 0.1D0
      PRINT *, S
      WRITE (S, '(ES50.40)') 0.25D0
      PRINT *, S
      WRITE (S, '(F60.50)') 0.5
      PRINT *, S
      WRITE (*, '(EN15.3,A)') 999.9999, 'z'
      WRITE (*, '(EN12.2,ES12.2,EN20.12)') 9.999E5, 9.999, 1.5
      END
