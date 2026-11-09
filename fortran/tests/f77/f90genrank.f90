! A generic interface picks its specific by the actuals' rank as well as
! their type (F90 14.1.2.3): explicit-shape dummies of rank 1 and 2, whole
! arrays and sections of each rank.  The rank was not looked at, and the
! rank-1 specific was called for every array.
module f90grk
  interface g
    module procedure g1, g2
  end interface
contains
  subroutine g1(x)
    real x(3)
    print *, 'rank 1', x
  end subroutine
  subroutine g2(x)
    real x(2,2)
    print *, 'rank 2', x
  end subroutine
end module
program f90genrank
  use f90grk
  real :: a(3), b(2,2), c(4,4)
  a = 1.0
  b = 2.0
  c = 3.0
  call g(a)
  call g(b)
  call g(c(1:2,2:3))
  call g(c(1:3,1))
end program
