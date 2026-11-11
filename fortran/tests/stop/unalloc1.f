C     A whole ALLOCATABLE array assigned before it is allocated: Fortran
C     2003 allocates it there, Fortran 95 does not, and f77 says so by
C     name.  (It stored nothing, and the next read of X was from address
C     0.)  An allocated one, and a section, go on as they did.
      PROGRAM UNALL
      REAL, ALLOCATABLE :: X(:), Y(:)
      ALLOCATE (Y(3))
      Y = 2.0
      Y(2:3) = 4.0
      PRINT *, Y
      X = Y
      PRINT *, 'not reached', X
      END
