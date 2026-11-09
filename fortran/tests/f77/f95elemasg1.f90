module mw
  implicit none
  type v2
    real :: x, y
  end type
  type cnt
    integer :: n
  end type
  interface assignment(=)
    module procedure v_from_r, r_from_v, c_from_c
  end interface
  interface operator(.dot.)
    module procedure vdot
  end interface
contains
  elemental subroutine v_from_r(a, r)
    type(v2), intent(out) :: a
    real, intent(in) :: r
    a%x = r; a%y = -r
  end subroutine
  elemental subroutine r_from_v(r, a)
    real, intent(out) :: r
    type(v2), intent(in) :: a
    r = a%x + a%y
  end subroutine
  elemental subroutine c_from_c(a, b)
    type(cnt), intent(out) :: a
    type(cnt), intent(in) :: b
    a%n = b%n + 1000
  end subroutine
  elemental real function vdot(a, b)
    type(v2), intent(in) :: a, b
    vdot = a%x * b%x + a%y * b%y
  end function
end module
program o3
  use mw
  implicit none
  type(v2) :: a(3), s
  type(cnt) :: c(3), d(3), e
  real :: f(3), g(3)
  integer :: i
  f = (/ 1.0, 2.0, 3.0 /)
  a = f
  print *, a
  a = 7.0
  print *, a
  a(2:3) = f(1:2) * 2.0
  print *, a
  g = a
  print *, g
  s = 5.0
  print *, s
  print *, s .dot. a
  print *, (s .dot. a) + f
  do i = 1, 3
    c(i)%n = i
  end do
  d = c
  print *, d
  e = c(2)
  print *, e
  d = e
  print *, d
  d(1:2) = c(2:3)
  print *, d
  where (f > 1.5) g = a
  print *, g
end program
