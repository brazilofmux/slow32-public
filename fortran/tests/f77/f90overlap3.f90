! Overlap through an array of structures' components, through
! EQUIVALENCE, in a constructor that names its target, and under an
! ELEMENTAL defined assignment between types.
module tm
  implicit none
  type v
    integer :: k
    real :: r
  end type
  type w
    integer :: n
  end type
  interface assignment(=)
    module procedure wfromi, wfromw2
  end interface
contains
  elemental subroutine wfromi(a, b)
    type(w), intent(out) :: a
    integer, intent(in) :: b
    a%n = b * 10
  end subroutine
  elemental subroutine wfromw2(a, b)
    integer, intent(out) :: a
    type(w), intent(in) :: b
    a = b%n + 1
  end subroutine
end module
program f90overlap3
  use tm
  implicit none
  type(v) :: xs(5)
  type(w) :: ws(5)
  integer :: i, a(6), b(6), m(5)
  real :: r(5)
  equivalence (a, b)
  do i = 1, 5
    xs(i)%k = i
    xs(i)%r = i * 1.5
  end do
  xs(2:5)%k = xs(1:4)%k
  print *, xs%k
  xs%k = (/ 1, 2, 3, 4, 5 /)
  xs%k = xs(5:1:-1)%k
  print *, xs%k
  xs(1:5)%r = xs(5:1:-1)%k + xs(5:1:-1)%r
  print *, xs%r
  xs%k = (/ 1, 2, 3, 4, 5 /)
  xs(2:5) = xs(1:4)
  print *, xs%k
  r = xs%r
  print *, r
  a = (/ (i, i = 1, 6) /)
  a(2:6) = b(1:5)
  print *, a
  a = (/ (i, i = 1, 6) /)
  b(1:5) = a(2:6) + a(6:2:-1)
  print *, b
  a = (/ (i, i = 1, 6) /)
  a = (/ a(6), a(5), a(4), a(3), a(2), a(1) /)
  print *, a
  a(1:3) = (/ b(3), b(2), b(1) /)
  print *, a
  a = (/ (a(7 - i), i = 1, 6) /)
  print *, a
  ws%n = (/ 1, 2, 3, 4, 5 /)
  m = (/ 1, 2, 3, 4, 5 /)
  ws(2:5) = m(1:4)
  print *, ws%n
  m(2:5) = ws(1:4)
  print *, m
  ws%n = (/ 1, 2, 3, 4, 5 /)
  ws(2:5) = ws(1:4)%n
  print *, ws%n
  ws(1:4) = ws(2:5)%n + ws(5:2:-1)%n
  print *, ws%n
  ws = ws(3)%n
  print *, ws%n
end program
