! DOUBLE PRECISION and COMPLEX locals as keyword actuals (X=D), which a
! procedure sets, and as the target of a pointer component.
module m2f
  implicit none
  type t
    real(8), pointer :: p
  end type
contains
  subroutine bump(n, x, z)
    integer :: n
    real(8) :: x
    complex, optional :: z
    x = x + n
    if (present(z)) z = z * 2
  end subroutine
  subroutine s(n)
    integer :: n
    real(8) :: d, e
    complex :: z
    d = 0.25d0 * n
    e = 0.5d0 * n
    z = cmplx(n, -n)
    call bump(n, x=d, z=z)
    call bump(x=e, n=2)
    print *, d, e, z
    d = d * 3
    print *, d, z
  end subroutine
  subroutine u(n)
    integer :: n
    real(8), target :: d
    type(t) :: v
    d = 0.25d0 * n
    v%p => d
    v%p = v%p + 2
    print *, d
    d = d * 2
    print *, v%p
    v = t(d)
    v%p = v%p + 2
    print *, d
  end subroutine
end module
program p2f
  use m2f
  call s(1)
  call s(3)
  call u(1)
  call u(5)
end program
