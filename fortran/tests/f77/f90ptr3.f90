! POINTER arguments (Fortran 90 12.4.1.1): a POINTER dummy gets the
! actual pointer itself (its association set by the procedure, a module's
! or an internal one); an ordinary dummy gets the pointer's target, an
! element of an array pointer's, or its elements.
module f90ptrpm3
contains
  subroutine aim(p, t)
    real, pointer :: p
    real, target :: t
    p => t
  end subroutine
end module
program f90ptr3
  use f90ptrpm3
  implicit none
  real, target :: x, y
  real, pointer :: p
  real, pointer :: v(:)
  real, target :: arr(4)
  x = 1.0
  y = 2.0
  call aim(p, y)
  print *, p, associated(p, y)
  call bump(p)
  print *, y
  call setto(p)
  print *, associated(p, x), p
  arr = 3.0
  v => arr
  call bump(v(2))
  call total(v, 4)
  print *, arr
contains
  subroutine bump(r)
    real :: r
    r = r + 10.0
  end subroutine
  subroutine setto(q)
    real, pointer :: q
    q => x
  end subroutine
  subroutine total(a, n)
    integer :: n
    real :: a(n)
    print *, sum(a)
  end subroutine
end program
