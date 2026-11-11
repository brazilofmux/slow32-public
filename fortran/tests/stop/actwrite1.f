      OPEN (10, STATUS='SCRATCH', ACTION='WRITE')
      WRITE (10, *) 1
      REWIND 10
      PRINT *, 'before'
      READ (10, *) I
      PRINT *, I
      END
