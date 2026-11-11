      CHARACTER*20 T
      T = ' 12 abc'
      PRINT *, 'before'
      READ (T, *) I, J
      PRINT *, I, J
      END
