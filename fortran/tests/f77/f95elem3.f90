module m
  implicit none
contains
  pure function sq(x) result(r)
    integer, intent(in) :: x
    integer :: r
    r = x * x
  end function
  elemental function cube(x) result(r)
    real, intent(in) :: x
    real :: r
    r = x * x * x
  end function
  elemental subroutine swap(a, b)
    integer, intent(inout) :: a, b
    integer :: t
    t = a; a = b; b = t
  end subroutine
  pure subroutine setit(a, v)
    integer, intent(out) :: a
    integer, intent(in) :: v
    a = v
  end subroutine
end module
program a2
  use m
  implicit none
  real :: x(3), y(2,2)
  integer :: p(3), q(3), k
  x = (/ 1.0, 2.0, 3.0 /)
  print *, sq(7), cube(2.0)
  print *, cube(x)
  y = reshape((/ 1.0, 2.0, 3.0, 4.0 /), (/ 2, 2 /))
  print *, cube(y) + 1.0
  print *, sum(cube(x(1:2)))
  p = (/ 1, 2, 3 /); q = (/ 7, 8, 9 /)
  call swap(p, q)
  print *, p, q
  call swap(p(1), q(1))
  print *, p, q
  call setit(k, 5)
  print *, k
end program
