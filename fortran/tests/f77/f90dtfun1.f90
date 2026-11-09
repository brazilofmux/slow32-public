! Functions whose result is a structure (Fortran 90 12.5.2.2): module,
! internal and external ones, TYPE(name) in the header, the result
! declared in the body, with RESULT, an external one declared by its
! type; their values in expressions, assignments, as actuals and nested
! in each other's actuals.
module f90dtvecm
  implicit none
  type v2
    real :: x, y
  end type
contains
  function add(a, b)
    type(v2), intent(in) :: a, b
    type(v2) :: add
    add%x = a%x + b%x
    add%y = a%y + b%y
  end function
  type(v2) function scale(a, f)
    type(v2), intent(in) :: a
    real :: f
    scale = v2(a%x * f, a%y * f)
  end function
  function mid(a, b) result(m)
    type(v2) :: a, b, m
    m = scale(add(a, b), 0.5)
  end function
end module
program f90dtfun1
  use f90dtvecm
  implicit none
  type(v2), external :: f90dtmk
  type(v2) :: p, q, r(2)
  p = v2(1.0, 2.0)
  q = add(p, v2(10.0, 20.0))
  print *, q, scale(q, 2.0)
  r(1) = mid(p, q)
  r(2) = flip(r(1))
  print *, r
  q = f90dtmk(3.0)
  print *, q, f90dtmk(1.0)
contains
  function flip(a)
    type(v2) :: a, flip
    flip = v2(a%y, a%x)
  end function
end program
function f90dtmk(t)
  use f90dtvecm
  real :: t
  type(v2) :: f90dtmk
  f90dtmk = v2(t, -t)
end function
