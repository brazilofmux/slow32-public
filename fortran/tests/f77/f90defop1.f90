module vecs
  implicit none
  type vec
    real :: x, y
  end type
  interface operator(+)
    module procedure vadd
  end interface
  interface operator(-)
    module procedure vsub, vneg
  end interface
  interface operator(*)
    module procedure vscale, vdot
  end interface
  interface operator(==)
    module procedure veq
  end interface
  interface operator(.cross.)
    module procedure vcross
  end interface
  interface operator(.nrm.)
    module procedure vn2
  end interface
contains
  function vadd(a, b) result(c)
    type(vec), intent(in) :: a, b
    type(vec) :: c
    c = vec(a%x + b%x, a%y + b%y)
  end function
  function vsub(a, b)
    type(vec), intent(in) :: a, b
    type(vec) :: vsub
    vsub = vec(a%x - b%x, a%y - b%y)
  end function
  function vneg(a)
    type(vec), intent(in) :: a
    type(vec) :: vneg
    vneg = vec(-a%x, -a%y)
  end function
  function vscale(s, a)
    real, intent(in) :: s
    type(vec), intent(in) :: a
    type(vec) :: vscale
    vscale = vec(s*a%x, s*a%y)
  end function
  real function vdot(a, b)
    type(vec), intent(in) :: a, b
    vdot = a%x*b%x + a%y*b%y
  end function
  logical function veq(a, b)
    type(vec), intent(in) :: a, b
    veq = a%x == b%x .and. a%y == b%y
  end function
  real function vcross(a, b)
    type(vec), intent(in) :: a, b
    vcross = a%x*b%y - a%y*b%x
  end function
  real function vn2(a)
    type(vec), intent(in) :: a
    vn2 = a%x**2 + a%y**2
  end function
end module

program t
  use vecs
  implicit none
  type(vec) :: p, q, r
  real :: d
  p = vec(1.0, 2.0)
  q = vec(3.0, -4.0)
  r = p + q
  print *, r%x, r%y
  r = p - q + p
  print *, r%x, r%y
  r = -p
  print *, r%x, r%y
  r = 2.0 * q
  print *, r%x, r%y
  d = p * q
  print *, d
  print *, p == q, p == p
  if (p + q == vec(4.0, -2.0)) print *, 'equal'
  print *, p .cross. q
  print *, .nrm. q + 1.0
  print *, 2.0 * (p + q) * q
end program
