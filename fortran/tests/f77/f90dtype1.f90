! Derived types (Fortran 90 4.4): definitions, nested; component
! references, of elements and of component arrays, CHARACTER and its
! substrings; structure constructors, nested, an array component's value
! from a constructor or a scalar; assignment of whole structures; a
! structure in a list-directed PRINT; a structure and a component as
! actual arguments (by address: the callee changes them).
program f90dtype1
  implicit none
  type point
    real :: x, y
  end type
  type seg
    type(point) :: a, b
    character(len=4) :: tag
    integer :: hits(3)
  end type
  type(point) :: p, q, pts(3)
  type(seg) :: s, ss(2)
  integer :: i
  p%x = 1.5
  p%y = -2.0
  q = p
  q%y = q%y * 3
  print *, p, q
  p = point(3.0, 4.0)
  print *, p%x, p%y, sqrt(p%x**2 + p%y**2)
  do i = 1, 3
    pts(i) = point(real(i), real(i * i))
  end do
  print *, pts(2)%y, pts
  s%a = p
  s%b = pts(3)
  s%tag = 'line'
  s%hits = (/ 7, 8, 9 /)
  s%hits(2) = s%hits(2) + 10
  print *, s
  print *, s%b%x, s%tag(2:3), s%hits(2:3), sum(s%hits)
  ss(1) = seg(point(1., 2.), point(3., 4.), 'one', (/ 1, 2, 3 /))
  ss(2) = seg(p, q, 'two', 0)
  ss(2)%a%x = 99.0
  print *, ss(1)%b%y, ss(2)%a, ss(2)%hits, ss(1)%tag // ss(2)%tag
  call move(p, 1.0, 1.0)
  call bump(ss(1)%hits(3))
  print *, p, ss(1)%hits
contains
  subroutine move(pt, dx, dy)
    type(point) :: pt
    real :: dx, dy
    pt%x = pt%x + dx
    pt%y = pt%y + dy
  end subroutine
  subroutine bump(k)
    integer :: k
    k = k + 100
  end subroutine
end program
