module mv
  implicit none
  type v2
    real :: x, y
  end type
  interface operator(+)
    module procedure vadd
  end interface
  interface operator(-)
    module procedure vneg, vsub
  end interface
  interface operator(*)
    module procedure vscale
  end interface
  interface operator(==)
    module procedure veq
  end interface
  interface operator(.dot.)
    module procedure vdot
  end interface
  interface operator(.len.)
    module procedure vlen
  end interface
contains
  elemental function vadd(a, b) result(r)
    type(v2), intent(in) :: a, b
    type(v2) :: r
    r%x = a%x + b%x; r%y = a%y + b%y
  end function
  elemental function vsub(a, b) result(r)
    type(v2), intent(in) :: a, b
    type(v2) :: r
    r%x = a%x - b%x; r%y = a%y - b%y
  end function
  elemental function vneg(a) result(r)
    type(v2), intent(in) :: a
    type(v2) :: r
    r%x = -a%x; r%y = -a%y
  end function
  elemental function vscale(s, a) result(r)
    real, intent(in) :: s
    type(v2), intent(in) :: a
    type(v2) :: r
    r%x = s * a%x; r%y = s * a%y
  end function
  elemental logical function veq(a, b)
    type(v2), intent(in) :: a, b
    veq = a%x == b%x .and. a%y == b%y
  end function
  elemental real function vdot(a, b)
    type(v2), intent(in) :: a, b
    vdot = a%x * b%x + a%y * b%y
  end function
  elemental real function vlen(a)
    type(v2), intent(in) :: a
    vlen = sqrt(a%x * a%x + a%y * a%y)
  end function
end module
program o2
  use mv
  implicit none
  type(v2) :: a(3), b(3), c(3), s
  real :: d(3), f(3)
  logical :: q(3)
  integer :: i
  do i = 1, 3
    a(i) = v2(real(i), real(2 * i))
    b(i) = v2(real(10 * i), 1.0)
  end do
  s = v2(3.0, 4.0)
  c = a + b
  print *, c
  c = a - b
  print *, c
  c = -a
  print *, c
  c = a + s
  print *, c
  c = s + a
  print *, c
  f = (/ 1.0, 2.0, 3.0 /)
  c = f * a
  print *, c
  c = 2.0 * a
  print *, c
  c = f * s
  print *, c
  c = 2.0 * (a + b) - s
  print *, c
  d = a .dot. b
  print *, d
  d = .len. a
  print *, d
  print *, .len. s, s .dot. s
  d = (a .dot. b) + f
  print *, d
  q = a == b
  print *, q
  q = a == a
  print *, q
  print *, a + b
  print *, a(2:3) + b(1:2)
  print *, sum(a .dot. b), maxval(.len. a)
  where (a == a) d = .len. (a + b)
  print *, d
  print *, a(3:1:-1) .dot. b
  print *, count(a + b == b + a)
end program
