      OPEN (10, STATUS='SCRATCH', ACTION='READ')
      PRINT *, 'before'
      WRITE (10, *) 1
      PRINT *, 'after'
      END
