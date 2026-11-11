      LOGICAL FUNCTION L1 (K)
      LOGICAL L2
      L1 = K .GT. 0
      RETURN
      ENTRY L2 (K)
      L2 = K .LT. 0
      END
      PROGRAM P
      LOGICAL L1, L2
      PRINT *, L1(1), L2(1), .NOT. L2(-1)
      END
