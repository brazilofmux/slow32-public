      CHARACTER*20 T
      DOUBLE PRECISION R
      T = '             end'
      PRINT *, 'before'
      READ (T, '(F20.0)') R
      PRINT *, R
      END
