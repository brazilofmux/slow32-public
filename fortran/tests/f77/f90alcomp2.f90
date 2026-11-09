module pm
  type poly
    real, allocatable :: c(:)
  end type
  interface operator(+)
    module procedure padd
  end interface
contains
  function padd(a, b) result(r)
    type(poly), intent(in) :: a, b
    type(poly) :: r
    allocate(r%c(size(a%c)))
    r%c = a%c + b%c
  end function
  function mk(v) result(r)
    real :: v
    type(poly) :: r
    allocate(r%c(64))
    r%c = v
  end function
end module
program t
  use pm
  type(poly) :: a, b, c
  integer :: i
  real :: s
  a = mk(1.0)
  b = mk(0.5)
  s = 0
  do i = 1, 200000
    c = a + b + b + mk(0.25)
    s = s + c%c(1)
    if (mod(i, 2) == 0) c = a
  end do
  print *, s, c%c(64)
end program
